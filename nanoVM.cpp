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

// Trap Codes
enum{
    TRAP_GETC = 0x20, // get character from keyboard, not echoed
    TRAP_OUT = 0x21, // output a character
    TRAP_PUTS = 0x22, // output a word string
    TRAP_IN = 0x23, // get character from keyboard, echoed
    TRAP_PUTSP = 0x24, // output a byte string
    TRAP_HALT = 0x25 // halt the program
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
                /*
                Addition
                Encodings : a) 0001 : DR : SR1 : 0 : 00 : SR2
                            b) 0001 : DR : SR1 : 1 : imm5
                Operatiom : if (bit[5] == 0)
                                DR = SR1 + SR2
                            else
                                DR = SR1 + sign_extend(imm5)
                            update_flags()
                */

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

                update_flags(r0);

                break;

            case OP_AND:
                /*
                Bitwise And
                Encodings : a) 0101 : DR : SR1 : 0 : 00 : SR2
                            b) 0101 : DR : SR1 : 1 : imm5
                Operation : if (bit[5] == 0)
                                DR = SR1 and SR2
                            else
                                DR = SR1 and sign_extend(imm5)
                            update_flags()
                */

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

                update_flags(r0);

                break;

            case OP_NOT:
                /*
                Bitwise Complement
                Encodings : 1001 : DR : SR : 1 : 11111
                Operation : DR = NOT(SR)
                            update_flags()
                */

                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t r1 = (instr >> 6) & 0x7;

                reg[r0] = ~reg[r1];

                update_flags[r0];

                break;

            case OP_BR:
                /*
                Conditional Branch
                Encodings : 0000 : n : z : p : PCoffset9
                Operation : if ((n and N) or (z and Z) or (p and P))
                                PC = PC + sign_extend(PCoffset9);
                
                Note : This is a PC relative branching, i.e. pc-> pc + offset
                Example : BRzp LOOP : Branch to LOOP if the last result was zero or positive
                */
                
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                uint16_t cond_flag = (instr >> 9) & 0x7;
                if (cond_flag & reg[R_COND]){
                    reg[R_PC] += pc_offset;
                } // end if

                break;

            case OP_JMP:
                /*
                Jump or Return from Subroutine
                Encoding : 1100 : 000 : BaseR : 000000
                Operation : PC = BaseR

                Note : when BaseR = 111 , i.e R7, the JMP is considered as RET,
                as RET usually loads PC with the contents of R7, where R7 contains the instruction
                following the subroutine call instruction.
                */

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
                
                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                reg[r0] = reg[R_PC] + pc_offset;
                update_flags(r0);

                break;

            case OP_ST:
                
                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                mem_write(reg[R_PC] + pc_offset, reg[r0]);

                break;

            case OP_STI:

                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                mem_write(mem_read(reg[R_PC] + pc_offset), reg[r0]);
        
                break;

            case OP_STR:
                
                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t r1 = (instr >> 6) & 0x7;
                uint16_t offset = sign_extend(instr & 0x3F, 6);
                mem_write(reg[r1] + offset, reg[r0]);

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

