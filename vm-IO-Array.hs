{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Array.MArray
import Data.Array.IO
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import Data.Word
import System.Environment

data VM = VM { ram       :: IOUArray Word16 Word16
             , stack     :: IORef [Word16]
             , registers :: IOUArray Word16 Word16
             , pc        :: IORef Word16
             , halt      :: IORef Bool
             , input     :: IORef String
             }

io = liftIO

readRam a = do ram <- asks ram; io $ readArray ram a
writeRam a v = do ram <- asks ram; io $ writeArray ram a v

pop = do
    stack <- asks stack
    vs <- io $ readIORef stack
    case vs of
        v:vs' -> io $ v <$ writeIORef stack vs'
        [] -> fail "empty stack"
push v = do stack <- asks stack; io $ modifyIORef' stack (v:)

getRegister r = do registers <- asks registers; io $ readArray registers r
setRegister r v = do registers <- asks registers; io $ writeArray registers r v

getPc = do pc <- asks pc; io $ readIORef pc
jump v = v `seq` do pc <- asks pc; io $ writeIORef pc v

getHalt = do halt <- asks halt; io $ readIORef halt
doHalt = do halt <- asks halt; io $ writeIORef halt True

next = do
    pc <- getPc
    readRam pc <* jump (succ pc)

operand = do
    w <- next
    if w < 32768 then
        return w
    else if w < 32768 + 8 then
        getRegister (w - 32768)
    else
        fail $ "invalid operand " ++ show w

register = do
    w <- next
    if w < 32768 || w >= 32768 + 8 then
        fail $ "invalid register " ++ show w
    else
        return (w - 32768)

getInput = do
    input <- asks input
    l <- io getLine
    case l of
        ':':command -> do
            case words command of
                ["save"] -> return ()
                ["restore"] -> return ()
                _ -> io $ putStrLn "invalid command"
            getInput
        _ -> io $ writeIORef input (l ++ "\n")

getCharacter = do
    input <- asks input
    empty <- io $ null <$> readIORef input
    when empty getInput
    c:cs <- io $ readIORef input
    io $ writeIORef input cs
    return c

data Opcode = Halt | Set | Push | Pop | Eq | Gt | Jmp | Jt | Jf | Add | Mult | Mod | And | Or | Not | Rmem | Wmem | Call | Ret | Out | In | Noop
            deriving (Show, Enum, Bounded)

execute Halt = doHalt
execute Set  = do a <- register; b <- operand; setRegister a b
execute Push = do a <- operand; push a
execute Pop  = do a <- register; v <- pop; setRegister a v
execute Eq   = do a <- register; b <- operand; c <- operand; setRegister a $ if b == c then 1 else 0
execute Gt   = do a <- register; b <- operand; c <- operand; setRegister a $ if b > c then 1 else 0
execute Jmp  = do a <- operand; jump a
execute Jt   = do a <- operand; b <- operand; when (a > 0) $ jump b
execute Jf   = do a <- operand; b <- operand; when (a == 0) $ jump b
execute Add  = do a <- register; b <- operand; c <- operand; setRegister a $ (b + c) `clearBit` 15
execute Mult = do a <- register; b <- operand; c <- operand; setRegister a $ (b * c) `clearBit` 15
execute Mod  = do a <- register; b <- operand; c <- operand; setRegister a $ b `mod` c
execute And  = do a <- register; b <- operand; c <- operand; setRegister a $ b .&. c
execute Or   = do a <- register; b <- operand; c <- operand; setRegister a $ b .|. c
execute Not  = do a <- register; b <- operand; setRegister a $ complement b `clearBit` 15
execute Rmem = do a <- register; b <- operand; v <- readRam b; setRegister a v
execute Wmem = do a <- operand; b <- operand; writeRam a b
execute Call = do a <- operand; ret <- getPc; push ret; jump a
execute Ret  = do ret <- pop; jump ret
execute Out  = do a <- operand; io $ putChar $ toEnum $ fromIntegral a
execute In   = do a <- register; c <- getCharacter; setRegister a $ fromIntegral $ fromEnum c
execute Noop = return ()

loop = do
    op <- next
    when (fromIntegral op > fromEnum (maxBound :: Opcode)) $ fail $ "invalid opcode " ++ show op
    execute (toEnum (fromIntegral op))
    halt <- getHalt
    unless halt loop

main = do
    [ramFile]   <- getArgs
    ramContents <- BS.readFile ramFile
    ram         <- newListArray (0, 32767) $ runGet (many getWord16le) ramContents
    stack       <- newIORef []
    registers   <- newArray (0, 7) 0
    pc          <- newIORef 0
    halt        <- newIORef False
    input       <- newIORef ""
    runReaderT loop VM {..}
