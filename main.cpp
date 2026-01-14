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

                // Destination register
                uint16_t r0 = (instr >> 9) & 0x7;
                // First operand SR1
                uint16_t r1 = (instr >> 5) & 0x7;
                // Whether Immediate mode
                uint16_t imm_flag = (instr >> 5) & 0x1;

                if (imm_flag){
                    uint16_t imm5 = sign_extend(instr & 0x1F, 5);
                    reg[r0] = reg[r1] + imm5;
                } // end if
                else{
                    // Second operand SR2
                    uint16_t r2 = instr & 0x7;
                    reg[r0] = reg[r1] + reg[r2];
                } // end else

                break;

            case OP_AND:
                // Loading registers and addressing flag
                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t r1 = (instr >> 6) & 0x7;
                uint16_t imm_flag = (instr >> 5) & 0x1;

                if (imm_flag){
                    uint16_t imm5 = sign_extend(instr & 0x1F, 5);
                    reg[r0] = reg[r1] & imm5;
                } // end if
                else{
                    uint16_t r2 = instr & 0x7;
                    reg[r0] = reg[r1] & reg[r2];
                } // end else

                break;

            case OP_NOT:
            
                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t r1 = (instr >> 6) & 0x7;

                reg[r0] = ~reg[r1];
                update_flags[r0];

                break;

            case OP_BR:
                
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                uint16_t cond_flag = (instr >> 9) & 0x7;
                if (cond_flag & reg[R_COND]){
                    reg[R_PC] += pc_offset;
                } // end if

                break;

            case OP_JMP:
                // Also handles RET, whats RET?
                uint16_t r1 = (instr >> 6) & 0x7;
                reg[R_PC] = reg[r1];
            
                break;

            case OP_JSR:

                uint16_t long_flag = (instr >> 11) & 1;
                reg[R_R7] = reg[R_PC];
                if (long_flag){
                    uint16_t long_pc_offset = sign_extend(instr & 0x7FF, 11);
                    reg[R_PC] += long_pc_offset;
                } // end if
                else{
                    uint16_t r1 = (instr >> 6) & 0x7;
                    reg[R_PC] = reg[r1];
                } // end else

                break;

            case OP_LD:
                
                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                reg[r0] = mem_read(reg[R_PC] + pc_offset);
                update_flags(r0);

                break;

            case OP_LDI:

                // Destination registor (DR)
                uint16_t r0 = (instr >> 9) & 0x7;

                // PC_Offset
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);

                // Add pc_offset to the current PC, look at that memory location to get
                // the final addres
                reg[r0] = mem_read(mem_read);
                update_flags(r0);
                
                break;

            case OP_LDR:

                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t r1 = (instr >> 6) & 0x7;
                uint16_t offset = sign_extend(instr & 0x3F, 6);
                reg[r0] = mem_read(reg[r1] + offset);
                update_flags(r0);

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


uint16_t sign_extend(uint16_t x, int bit_count){
    // If negative, shift the 1111111111111111 by bit count and OR
    if ((x >> (bit_count - 1)) & 1){
        x |= (0xFFFF << bit_count);
    } // end if

} // end function sign_extend

void update_flags(uint16_t r){

    if (reg[r] == 0){
        reg[R_COND] = FL_ZRO;
    } // end if
    else if (reg[r] >> 15){
        reg[R_COND] = FL_NEG;
    } // end else if
    else {
        reg{R_COND} = FL_POS;
    } // end else

} // end function update_flags

