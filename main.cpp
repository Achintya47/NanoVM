#include <cstdint>


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
};

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
}