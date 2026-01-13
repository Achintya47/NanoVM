#include <cstdint>
#include <iostream>

// Storage
#define MEMORY_MAX (1 << 16)
uint16_t memory[MEMORY_MAX];

// Registers (10 - 16 bits each)
enum{

    // General Purpose Registers
    R_R0 = 0,
    R_R1,
    R_R2,
    R_R3,
    R_R4,
    R_R5,
    R_R6,
    R_R7,

    // Program Counter
    R_PC,

    // Condition Flag
    R_COND,
    R_COUNT
};// end enum

// Registers in an array
uint16_t reg[R_COUNT];

// Opcodes (4-Bit each)
enum{

    OP_BR = 0, // branch
    OP_ADD, // add
    OP_LD, // load
    OP_ST, // store
    OP_JSR, // jump register
    OP_AND, // bitwise and
    OP_LDR, // bitwise or
    OP_STR, // store register
    OP_RTI, // --
    OP_NOT, // bitwise not
    OP_LDI, // load indirect
    OP_STI, // store indirect
    OP_JMP, // jump
    OP_RES, // reserved
    OP_LEA, // load effective address
    OP_TRAP // execute trap
}; // end enum

// Condition flags
enum{
    FL_POS =  1 << 0, // P
    FL_ZRO = 1 << 1, // Z
    FL_NEG = 1 << 2 // N
}; // end enum


int main(int argc, const char* argv[]){

    if (argc < 2) {
        std::cout << "LC3 [image-file1 ..\n]" << std::endl;
        exit(2);
    } // end if

    for (int j = 1; j < argc; j++){
        if (!read_image(argv[j])){
            std::cout << "Failed to load image : %s\n" << argv[j] << std::endl;
            exit(1);
        } // end if
    } // end for
    @{Setup}

    reg[R_COND] = FL_ZRO;
    enum {PC_START = 0x3000};
    reg[R_PC] = PC_START;

    int running = 1;
    while (running){

        uint16_t instr = mem_read(reg[R_PC]++);
        uint16_t op = instr >> 12;
        
        switch (op){
            case OP_ADD:
                @{ADD}
                break;
            case OP_AND:
                @{AND}
                break;
            case OP_NOT:
                @{NOT}
                break;
            case OP_BR:
                @{BR}
                break;
            case OP_JMP:
                @{JMP}
                break;
            case OP_JSR:
                @{JSR}
                break;
            case OP_LD:
                @{LD}
                break;
            case OP_LDI:
                @{LDI}
                break;
            case OP_LDR:
                @{LDR}
                break;
            case OP_LEA:
                @{LEA}
                break;
            case OP_ST:
                @{ST}
                break;
            case OP_STI:
                @{STI}
                break;
            case OP_STR:
                @{STR}
                break;
            case OP_TRAP:
                @{TRAP}
                break;
            case OP_RES:
            case OP_RTI:
            default:
                @{BAD OPCODE}
                break;

        } // end switch
    } // end while
    @{Shutdown}
}