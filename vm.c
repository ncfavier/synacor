#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#define MEM_SIZE 32768
#define STACK_SIZE 100000

#define NEXT mem[pc++]
#define OPERAND operand(NEXT)
#define REGISTER register_(NEXT)

typedef uint16_t num;

num mem[MEM_SIZE];
num reg[8];
num stack[STACK_SIZE];
num pc = 0;
size_t sp = 0;
int halt = 0;

num operand(num op) {
    if (op < 32768)
        return op;
    else if (op < 32768 + 8)
        return reg[op - 32768];
    else {
        fprintf(stderr, "invalid operand %d\n", op);
        exit(1);
    }
}

size_t register_(num r) {
    if (32768 <= r && r < 32768 + 8)
        return r - 32768;
    else {
        fprintf(stderr, "invalid register %d\n", r);
        exit(1);
    }
}

void push(num val) {
    if (sp >= STACK_SIZE) {
        fprintf(stderr, "stack overflow\n");
        exit(1);
    }

    stack[sp++] = val;
}

num pop() {
    if (sp == 0) {
        fprintf(stderr, "empty stack\n");
        exit(1);
    }

    return stack[--sp];
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s PROGRAM\n", argv[0]);
        return 1;
    }

    FILE *program = fopen(argv[1], "rb");

    if (!program) {
        perror(argv[1]);
        return 1;
    }

    unsigned char b[2];
    num i = 0;

    while (fread(b, 2, 1, program) != 0)
        mem[i++] = b[0] | (b[1] << 8);

    while (!halt) {
        num op = NEXT, a, b, c;
        int ch;

        switch (op) {
            case 0: // halt
                halt = 1;
                break;
            case 1: // set
                a = REGISTER, b = OPERAND;
                reg[a] = b;
                break;
            case 2: // push
                a = OPERAND;
                push(a);
                break;
            case 3: // pop
                a = REGISTER;
                reg[a] = pop();
                break;
            case 4: // eq
                a = REGISTER, b = OPERAND, c = OPERAND;
                reg[a] = b == c;
                break;
            case 5: // gt
                a = REGISTER, b = OPERAND, c = OPERAND;
                reg[a] = b > c;
                break;
            case 6: // jmp
                a = OPERAND;
                pc = a;
                break;
            case 7: // jt
                a = OPERAND, b = OPERAND;
                if (a)
                    pc = b;
                break;
            case 8: // jf
                a = OPERAND, b = OPERAND;
                if (!a)
                    pc = b;
                break;
            case 9: // add
                a = REGISTER, b = OPERAND, c = OPERAND;
                reg[a] = (b + c) % 32768;
                break;
            case 10: // mult
                a = REGISTER, b = OPERAND, c = OPERAND;
                reg[a] = (b * c) % 32768;
                break;
            case 11: // mod
                a = REGISTER, b = OPERAND, c = OPERAND;
                reg[a] = b % c;
                break;
            case 12: // and
                a = REGISTER, b = OPERAND, c = OPERAND;
                reg[a] = b & c;
                break;
            case 13: // or
                a = REGISTER, b = OPERAND, c = OPERAND;
                reg[a] = b | c;
                break;
            case 14: // not
                a = REGISTER, b = OPERAND;
                reg[a] = ~b & ~(1 << 15);
                break;
            case 15: // rmem
                a = REGISTER, b = OPERAND;
                reg[a] = mem[b];
                break;
            case 16: // wmem
                a = OPERAND, b = OPERAND;
                mem[a] = b;
                break;
            case 17: // call
                a = OPERAND;
                push(pc);
                pc = a;
                break;
            case 18: // ret
                pc = pop();
                break;
            case 19: // out
                a = OPERAND;
                putchar(a);
                break;
            case 20: // in
                a = REGISTER;
                ch = getchar();
                if (ch == EOF)
                    halt = 1;
                else
                    reg[a] = (num) ch;
                break;
            case 21: // noop
                break;
            default:
                fprintf(stderr, "invalid opcode %d\n", op);
                return 1;
        }
    }

    return 0;
}
