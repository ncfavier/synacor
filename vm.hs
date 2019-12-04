{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import Data.List
import Data.Word
import System.Environment
import System.Exit
import System.IO

io = liftIO

data VM = VM { ram       :: UArray Word16 Word16
             , stack     :: [Word16]
             , registers :: UArray Word16 Word16
             , pc        :: Word16
             , halt      :: Bool
             , input     :: String
             , saves     :: [(String, VM)]
             }

readRam a = (! a) <$> gets ram
writeRam a v = modify' \vm -> vm { ram = ram vm // [(a, v)] }

pop = do
    stack <- gets stack
    case uncons stack of
        Just (v, stack') -> v <$ modify' \vm -> vm { stack = stack' }
        Nothing -> fail "empty stack"
push v = modify' \vm -> vm { stack = v:stack vm }

getRegister r = (! r) <$> gets registers
setRegister r v = v `seq` modify' \vm -> vm { registers = registers vm // [(r, v)] }

getPc = gets pc
jump v = modify' \vm -> vm { pc = v }

getHalt = gets halt
doHalt = modify' \vm -> vm { halt = True }

getInput = gets input
setInput s = modify' \vm -> vm { input = s }
addInput s = modify' \vm -> vm { input = input vm ++ s }

save n = do
    modify' \vm -> vm { saves = (n, vm { input = [], saves = [] }):filter ((/= n) . fst) (saves vm) }
    io $ putStrLn $ "saved " ++ n

restore Nothing = do
    saves <- gets saves
    case saves of
        (_, v):vs -> do
            put v { saves = saves }
            addInput "look\n"
        [] -> io $ putStrLn "no save to restore"

restore (Just n) = do
    saves <- gets saves
    case lookup n saves of
        Just v -> do
            put v { saves = saves }
            addInput "look\n"
        Nothing -> io $ putStrLn $ "no such save " ++ n

listSaves = do saves <- gets saves; io $ putStrLn $ "saves: " ++ unwords (map fst saves)

getCharacter = do
    input <- getInput
    if null input then
        readInput
    else do
        c:cs <- getInput
        setInput cs
        return c

commands = ["init", "save", "restore", "list", "quit"]

readInput = do
    l <- io $ putStr "> " >> hFlush stdout >> getLine
    case l of
        ':':command -> do
            let cmd:args = words command
            case (filter (cmd `isPrefixOf`) commands, args) of
                (["init"], []) -> addInput $ concatMap (++ "\n") ["doorway", "north", "north", "bridge", "continue", "down", "east", "take empty lantern", "west"]
                (["save"], [n]) -> save n
                (["restore"], []) -> restore Nothing
                (["restore"], [n]) -> restore (Just n)
                (["list"], []) -> listSaves
                (["quit"], []) -> io exitSuccess
                _ -> io $ putStrLn "invalid command"
        _ -> addInput (l ++ "\n")
    getCharacter

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

loop = forever do
    op <- next
    when (fromIntegral op > fromEnum (maxBound :: Opcode)) $ fail $ "invalid opcode " ++ show op
    execute (toEnum (fromIntegral op))
    halt <- getHalt
    when halt do
        io $ putStrLn ""
        listSaves
        l <- io $ putStr "restore> " >> hFlush stdout >> getLine
        restore case words l of
            n:_ -> Just n
            [] -> Nothing

main = do
    [ramFile]   <- getArgs
    ramContents <- BS.readFile ramFile
    let vm = VM { ram       = listArray (0, 32767) $ runGet (many getWord16le) ramContents
                , stack     = []
                , registers = listArray (0, 7) (repeat 0)
                , pc        = 0
                , halt      = False
                , input     = ""
                , saves     = []
                }
    runStateT loop vm
