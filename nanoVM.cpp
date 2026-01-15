/*
===============================================================================
 nanoVM.cpp — LC-3 Virtual Machine Implementation
===============================================================================

OVERVIEW
--------
This file implements a complete LC-3 (Little Computer 3) virtual machine
in C++. The LC-3 is a 16-bit educational architecture designed to teach
computer organization, instruction sets, memory models, and system calls.

This VM emulates:
- A 16-bit word-addressed memory model
- LC-3 registers and condition flags
- The LC-3 instruction set (ADD, AND, BR, LD, ST, JSR, etc.)
- Memory-mapped I/O for keyboard input
- LC-3 TRAP routines (GETC, OUT, PUTS, IN, PUTSP, HALT)
- Big-endian LC-3 object file loading on a little-endian host
- A fetch–decode–execute execution loop

This is a *hosted emulator*, not a hardware simulator. The guest (LC-3)
program is treated as untrusted input and must not be allowed to corrupt
host memory or state.

-------------------------------------------------------------------------------

ARCHITECTURE MODEL
------------------

1. MEMORY
   - LC-3 memory is word-addressed.
   - Each memory location stores one 16-bit word.
   - MEMORY_MAX = 2^16 words (addresses 0x0000 – 0xFFFF).
   - Implemented as: uint16_t memory[MEMORY_MAX]

2. REGISTERS
   - 8 General-purpose registers: R0–R7
   - Program Counter: PC
   - Condition Register: COND (N, Z, P flags)
   - Stored in a single array for compactness:
       uint16_t reg[R_COUNT]

3. CONDITION FLAGS
   - FL_NEG: Negative (bit 15 set)
   - FL_ZRO: Zero
   - FL_POS: Positive
   - Updated after arithmetic/logical instructions.

4. MEMORY-MAPPED I/O
   - Keyboard is accessed via special memory addresses:
       MR_KBSR (0xFE00): Keyboard Status Register
       MR_KBDR (0xFE02): Keyboard Data Register
   - Reading these addresses triggers host-side input polling.
   - This mimics LC-3 hardware behavior.

-------------------------------------------------------------------------------

ENDIANNESS
----------
LC-3 object (.obj) files are stored in BIG-ENDIAN format.
Most modern hosts (x86, Windows) are LITTLE-ENDIAN.

Therefore:
- Every 16-bit word read from an image file must be byte-swapped.
- swap16() is used to convert file data into host representation.
- Once loaded, all VM execution operates on host-endian uint16_t values.

-------------------------------------------------------------------------------

EXECUTION MODEL
---------------
The VM runs a classic fetch–decode–execute loop:

1. FETCH
   - Instruction is read from memory at PC.
   - PC is incremented immediately.

2. DECODE
   - The top 4 bits of the instruction select the opcode.

3. EXECUTE
   - Instruction semantics are implemented via a switch(opcode).
   - PC-relative addressing, register addressing, and indirect
     addressing follow the LC-3 ISA specification exactly.

4. UPDATE FLAGS
   - Arithmetic and logical instructions update condition flags.

-------------------------------------------------------------------------------

TRAP HANDLING
-------------
TRAP instructions provide system services.
This VM implements them directly (without an LC-3 OS):

- TRAP_GETC  (0x20): Read one character, no echo
- TRAP_OUT   (0x21): Output one character
- TRAP_PUTS  (0x22): Output string (1 char per word)
- TRAP_IN    (0x23): Prompt and echo one character
- TRAP_PUTSP (0x24): Output packed string (2 chars per word)
- TRAP_HALT  (0x25): Stop execution

TRAP routines:
- Use only the low 8 bits of characters
- Explicitly mask and cast to avoid signed-char bugs
- Flush output to behave interactively

-------------------------------------------------------------------------------

INPUT HANDLING (WINDOWS)
------------------------
- Console input buffering is disabled to emulate raw keyboard input.
- Keyboard polling is non-blocking.
- Uses Windows APIs:
    - GetConsoleMode / SetConsoleMode
    - WaitForSingleObject
    - _kbhit()
- This design mirrors LC-3 polling via KBSR/KBDR.

-------------------------------------------------------------------------------

SAFETY & DESIGN NOTES
---------------------
- Memory access is centralized through mem_read() and mem_write().
- These functions form the VM's security boundary.
- Guest programs must never access memory out of bounds.
- Some abort() calls are placeholders and should eventually be replaced
  with a structured VM fault-handling system.

-------------------------------------------------------------------------------

LIMITATIONS / INTENTIONAL SIMPLIFICATIONS
-----------------------------------------
- No full LC-3 operating system is emulated.
- TRAP vectors are handled inline rather than via vector table jumps.
- Interrupts, privilege modes, and RTI are not implemented.
- This VM is designed for correctness, clarity, and learning—not speed.

-------------------------------------------------------------------------------

INTENDED USE
------------
This file is intended for:
- Learning computer architecture
- Studying instruction set emulation
- Building debuggers and fault systems
- Extending into a more advanced VM or teaching OS concepts

-------------------------------------------------------------------------------

AUTHOR NOTES
------------
This VM follows the LC-3 ISA specification closely and prioritizes:
- Deterministic behavior
- Explicit bit manipulation
- Clear mapping from ISA → implementation
- Host safety

Every subtle-looking detail (masking, sign extension, bounds checks)
exists for a reason.
Also the name was inspired from Andrej Karpathy's nanoGPT. 

===============================================================================
*/


#include <cstdint>
#include <iostream>
#include <Windows.h>
#include <conio.h>
#include <signal.h>

HANDLE hStdin = INVALID_HANDLE_VALUE;
DWORD fdwMode, fdwOldMode;

// Ring Buffer for VM's Program Counter
constexpr size_t TRACE_DEPTH = 32;
uint16_t pc_trace[TRACE_DEPTH];
size_t pc_trace_index = 0;

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

// Memory Mapped Registers (polling)
enum{
    MR_KBSR = 0xFE00, // Keyboard Status
    MR_KBDR = 0xFE02 // Keyboard Data
}; // end enum

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

// Forward declaration of fault
void fault(VmFaultType, const std::string&, uint16_t);

void disable_input_buffering(){
    hStdin = GetStdHandle(STD_INPUT_HANDLE);
    GetConsoleMode(hStdin, &fdwOldMode); // Save old mode

    fdwMode = fdwOldMode ^ ENABLE_ECHO_INPUT ^ ENABLE_LINE_INPUT;

    SetConsoleMode(hStdin, fdwMode);
    FlushConsoleInputBuffer(hStdin);

} // end function disable_input_buffering

void restore_input_buffering(){
    SetConsoleMode(hStdin, fdwOldMode);
} // end function restore_input_buffering

 uint16_t check_key(){
    return WaitForSingleObject(hStdin, 0) == WAIT_OBJECT_0 && _kbhit();
 } // end function check_key

 void handle_interrupt(int signal)
{
    restore_input_buffering();
    printf("\n");
    exit(-2);
}

void mem_write(uint16_t address, uint16_t val){
    if (address >= MEMORY_MAX) 
        fault(VmFaultType::InvalidMemoryWrite,
            "Write Outisde memory bounds",
            address);
    memory[address] = val;
} // end function mem_write

/**
 * @brief Function to read from the memory, Keyboard polled only when 
 * accessed
 * 
 * @param Address of the memory to be read
 */
uint16_t mem_read(uint16_t address){
    if (address >= MEMORY_MAX) 
        fault(VmFaultType::InvalidMemoryRead, 
            "Read outside memory bounds",
            address);

    if (address == MR_KBSR){
        if (check_key()){
            int ch = getchar();
            if (ch == EOF)  
                fault(VmFaultType::EOFOnInput,
                    "EOF while reading keyboard");

            memory[MR_KBSR] = (1 << 15);
            memory[MR_KBDR] = static_cast<uint16_t>(ch & 0xFF);
        } // end if
        else{
            memory[MR_KBSR] = 0;
        } // end else
    } // end if
    return memory[address];
 } // end function mem_read


uint16_t sign_extend(uint16_t x, int bit_count){
    // If negative, shift the 1111111111111111 by bit count and OR
    if ((x >> (bit_count - 1)) & 1){
        x |= (0xFFFF << bit_count);
    } // end if
    return x;

} // end function sign_extend

/**
 * @brief Updates the Flag based on the value stored in the provided register
 * to FL_ZRO, FL_NEG or FL_POS
 * 
 * @param A 16-bit address of the register
 */
void update_flags(uint16_t r){

    /*
    Uptade the flags w.r.t to the result stored in register r
    */

    if (reg[r] == 0){
        reg[R_COND] = FL_ZRO;
    } // end if
    else if (reg[r] >> 15){
        reg[R_COND] = FL_NEG;
    } // end else if
    else {
        reg[R_COND] = FL_POS;
    } // end else

} // end function update_flags
// Swap Endians
inline uint16_t swap16(uint16_t x){
    return (x << 8) | (x >> 8);
} // end function swap16

/**
 * @brief Function to load an LC-3 image file (.obj) into the VM's memory
 * following little-endian format
 * 
 * @param Takes an already opened LC-3 image file
 */
void read_image_file(FILE * file){

    // The origin is Big-Endian, swap to little-endian
    uint16_t origin;
    // Adding fread error checking
    if( fread(&origin, sizeof(origin), 1, file) != 1 )
        abort();
    
    origin = swap16(origin);

    if (origin >= MEMORY_MAX)
        abort();
    
    // Compute how much we can read, prevents writing past VM memory
    uint16_t max_read = MEMORY_MAX - origin;
    // Point to destination in VM memory
    uint16_t * p = memory + origin;
    // Read rest of the file, in Big-Endian
    size_t read = fread(p, sizeof(uint16_t), max_read, file);

    // Swap to Little-Endian
    while (read-- > 0){
        *p = swap16(*p);
        ++p;
    } // end while

} // end function read_image_file

enum class VmFaultType {
    InvalidMemoryRead,
    InvalidMemoryWrite,
    InvalidOpcode,
    InvalidTrap,
    UnterminatedString,
    IOError,
    EOFOnInput,
    InternalError
};

const char* fault_type_to_string(VmFaultType type) {
    switch (type) {
        case VmFaultType::InvalidMemoryRead:  return "Invalid memory read";
        case VmFaultType::InvalidMemoryWrite: return "Invalid memory write";
        case VmFaultType::UnterminatedString: return "Unterminated string";
        case VmFaultType::InvalidOpcode:      return "Invalid opcode";
        case VmFaultType::InvalidTrap:        return "Invalid trap";
        case VmFaultType::IOError:             return "I/O error";
        case VmFaultType::EOFOnInput:          return "EOF on input";
        case VmFaultType::InternalError:       return "Internal VM error";
        default:                               return "Unknown fault";
    }
}

void print_pc_trace() {
    fprintf(stderr, "\nLast %zu instructions (oldest → newest):\n", TRACE_DEPTH);

    size_t idx = pc_trace_index;
    for (size_t i = 0; i < TRACE_DEPTH; ++i) {
        uint16_t pc = pc_trace[idx];
        fprintf(stderr, "  [%02zu] PC = 0x%04X\n", i, pc);
        idx = (idx + 1) % TRACE_DEPTH;
    }
}


struct VMStateSnapshot {
    uint16_t pc;
    uint16_t reg[R_COUNT];
    uint16_t cond;
    uint64_t instruction_count;
    VmFaultType fault_type;
    uint16_t fault_address;
    uint16_t faulting_instruction;

    std::string message;
};

void fault(VmFaultType type, const std::string& msg, uint16_t fault_addr = 0){

    restore_input_buffering();

    VMStateSnapshot snap;
    snap.pc = reg[R_PC];
    snap.cond = reg[R_COND];
    snap.instruction_count = reg[R_COUNT];
    snap.fault_type = type;
    snap.fault_address = fault_addr,
    snap.faulting_instruction = memory[reg[R_PC]];
    snap.message = msg;

    for( int i = 0; i < R_COUNT; i++)
        snap.reg[i] = reg[i];

    fprintf(stderr, "\n================ LC-3 VM FAULT ================\n");
    fprintf(stderr, "Reason       : %s\n", fault_type_to_string(type));
    fprintf(stderr, "Message      : %s\n", msg);
    fprintf(stderr, "PC           : 0x%04X\n", snap.pc);
    fprintf(stderr, "Instruction  : 0x%04X\n", snap.faulting_instruction);

    if (fault_addr)
        fprintf(stderr, "Fault Address: 0x%04X\n", fault_addr);

    fprintf(stderr, "\nRegisters:\n");
    for (int i = 0; i < 8; ++i)
        fprintf(stderr, "R%d = 0x%04X\n", i, snap.reg[i]);

    fprintf(stderr, "PC   = 0x%04X\n", snap.reg[R_PC]);
    fprintf(stderr, "COND = 0x%04X\n", snap.reg[R_COND]);

    print_pc_trace();
    
    std::exit(EXIT_FAILURE);

} // end function fault


// Function to simple load the LC-3 file
int read_image(const char* image_path){
    
    // Open the LC-3 image
    FILE * file = fopen(image_path, "rb");
    if (!file) return 0;
    read_image_file(file);
    fclose(file);

    return 1;

} // end function read_image

int main(int argc, const char* argv[]){

    signal(SIGINT, handle_interrupt);
    disable_input_buffering();

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

    reg[R_COND] = FL_ZRO;
    enum {PC_START = 0x3000};
    reg[R_PC] = PC_START;

    int running = 1;
    while (running){

        pc_trace[pc_trace_index] = reg[R_PC];
        pc_trace_index = (pc_trace_index + 1) % TRACE_DEPTH;

        uint16_t instr = mem_read(reg[R_PC]++);
        uint16_t op = instr >> 12;
        
        switch (op){
            case OP_ADD: {
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
                uint16_t r1 = (instr >> 6) & 0x7;
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
            } // end Case OP_ADD
            case OP_AND: {
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
            } // end Case OP_AND
            case OP_NOT: {
                /*
                Bitwise Complement
                Encoding : 1001 : DR : SR : 1 : 11111
                Operation : DR = NOT(SR)
                            update_flags()
                */

                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t r1 = (instr >> 6) & 0x7;

                reg[r0] = ~reg[r1];

                update_flags(r0);

                break;
            } // end Case OP_NOT
            case OP_BR: {
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
            } // end Case OP_BR
            case OP_JMP: {
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
            } // end Case OP_JMP
            case OP_JSR: {
                /*
                Jump to Subroutine
                Encodings : a) 0100 : 1 : PCoffset11
                            b) 0100 : 0 : 00 : BaseR : 000000
                Operation : R7 = PC
                            if (bit[11] == 0)
                                PC = BaseR
                            else
                                PC = PC + sign_extend(PCoffset11)

                Note : Just like mentioned above, PC's following value is stored
                in R7, which will be loaded during RET
                */
                
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
            } // end Case OP_JSR
            case OP_LD: {
                /*
                Load
                Encoding : 0010 : DR : PCoffset9
                Operation : DR = mem[PC + sign_extend(PCoffset9)]
                            update_flags()
                
                Example : LD R4, VALUE : R4 <- mem[VALUE]
                */

                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                reg[r0] = mem_read(reg[R_PC] + pc_offset);

                update_flags(r0);

                break;
            } // end Case OP_LD
            case OP_LDI: {
                /*
                Load Indirect
                Encoding : 1010 : DR : PCoffset9
                Operation : DR = mem[mem[PC + sign_extend[PCoffset9]]]
                            update_flags()
                
                Example : LDI R4, NOT_HERE : R4 <- mem[mem[ONEMORE]]
                    Where,  NOT_HERE -> | HERE|
                            HERE     -> |  32 | 
                */

                // Destination registor (DR)
                uint16_t r0 = (instr >> 9) & 0x7;

                // PC_Offset
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);

                // Add pc_offset to the current PC, look at that memory location to get
                // the final addres
                reg[r0] = mem_read(mem_read(reg[R_PC] + pc_offset));

                update_flags(r0);
                
                break;
            } // end Case OP_LDI
            case OP_LDR: {
                /*
                Load Base + Offset
                Encoding : 0110 : DR : BaseR : offset6
                Operation : DR = mem[BaseR + sign_extend(offset6)]
                            update_flags()
                
                Example : LDR R4, R2, -5 : R4 <- mem[R2 - 5]
                */

                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t r1 = (instr >> 6) & 0x7;
                uint16_t offset = sign_extend(instr & 0x3F, 6);
                reg[r0] = mem_read(reg[r1] + offset);

                update_flags(r0);

                break;
            } // end Case OP_LDR
            case OP_LEA: {
                /*
                Load Effective Address
                Encoding : 1110 : DR : PCoffset9
                Operation : DR = PC + sign_extend(PCoffset9)
                            update_flags()
                
                Note : The offset addet to PC, and the resulting address is stored 
                in DR.
                Example : LEA R4, TARGET : R4 <- address of TARGET
                */
                
                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                reg[r0] = reg[R_PC] + pc_offset;

                update_flags(r0);

                break;
            } // end Case OP_LEA
            case OP_ST: {
                /*
                Store
                Encoding : 0011 : SR : PCoffset9
                Operation : mem[PC + sign_extend(PCoffset9)] = SR

                Example : ST R4, HERE : mem[HERE] <- R4
                */
                
                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                mem_write(reg[R_PC] + pc_offset, reg[r0]);

                break;
            } // end Case OP_ST
            case OP_STI: {
                /*
                Store Indirect
                Encoding : 1011 : SR : PCoffset9
                Operation : mem[mem[PC + sign_extend[PCoffset9]]] = SR

                Example : ST R4, NOT_HERE : mem[mem[NOT_HERE]] <- R4
                    Where,  NOT_HERE -> | HERE|
                            HERE     -> |  R4 | 
                */

                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t pc_offset = sign_extend(instr & 0x1FF, 9);
                mem_write(mem_read(reg[R_PC] + pc_offset), reg[r0]);
        
                break;
            } // end Case OP_STI
            case OP_STR: {
                /*
                Store Base + Offset
                Encoding : 0111 : SR : BaseR : offset6
                Operation : mem[BaseR + sign_extend(offset6)] = SR

                Example : STR R4, R2, -5 : mem[R2 - 5] <- R4
                */
                
                uint16_t r0 = (instr >> 9) & 0x7;
                uint16_t r1 = (instr >> 6) & 0x7;
                uint16_t offset = sign_extend(instr & 0x3F, 6);
                mem_write(reg[r1] + offset, reg[r0]);

                break;
            } // end Case OP_STR
            case OP_TRAP: {
                /*
                System Call
                Encoding : 1111 : 0000 : trapvect8
                Operation : R7 = PC
                            PC = mem[zero_extend[trapvect8]]
                
                Note : Memory locations x0000 through x00FF, 256 in all are available
                to contain syste, calls specified by their trap vectors
                Example : Trap x23 : Directs OS to execute IN system call,
                                     The starting address of this system call is
                                     contained in memory location x0023.
                */
                
                reg[R_R7] = reg[R_PC];

                switch (instr & 0xFF){
                    case TRAP_GETC: {
                        /*
                        Read one character, no Echo, character returned in R0

                        Note : Again, LC3 implementation isn't type-safe, thus when an EOF
                        character is returned (-1), R_R0's value becomes 0xFFFF, thus our character
                        space is violated.
                        */

                        int ch = getchar();
                        reg[R_R0] = static_cast<uint16_t>(ch & 0xFF);

                        update_flags(R_R0);

                        break;
                    } // end Case TRAP_GETC
                    case TRAP_OUT: {
                        /*
                        Immediately flush the character to the output,
                        like an interactive session.

                        Note : Type Safety Implemented
                        */
                        putc(static_cast<unsigned char>(reg[R_R0] & 0xFF), stdout);
                        fflush(stdout);

                        break;
                    } // end Case TRAP_OUT
                    case TRAP_PUTS: {
                        /*
                        Display a string stored in consecutive memory locations
                        character by character until x0000 encountered

                        Note : LC3 implementation uses C-style cast, unsafe as it depends on
                        the compiler and the character set of your terminal, a better version is
                        static casting to an unsigned character (0 -> 255). 
                        Its 'Sweeter - Better - Bolder'.
                        */
                        {

                        if(reg[R_R0] >= MEMORY_MAX)
                            fault(VmFaultType::UnterminatedString,
                                "PUTS string not Null-Terminated",
                                reg[R_R0]);

                        uint16_t* c = memory + reg[R_R0];
                            while(*c){
                                putc(static_cast<unsigned char>(*c & 0xFF), stdout);
                                ++c;
                            } // end while
                        } // end PUTS block

                        break;
                    } // end Case TRAP_PUTS
                    case TRAP_IN: {
                        /*
                        Prompt for Input Character (with type safety)
                        */

                        printf("Enter a character : ");
                        fflush(stdout);

                        int ch = getchar();
                        putc(ch, stdout);
                        fflush(stdout);

                        reg[R_R0] = static_cast<uint16_t>(ch & 0xFF);
                        update_flags(R_R0);

                        break;
                    } // end Case TRAP_IN
                    case TRAP_PUTSP: {
        
                            /*
                            One char per byte (two bytes per word)
                            extract low and high bytes per LC-3 PUTSP spec
                            Word Bits : 
                                [high byte][low byte]

                            Note :  char1 -> Low Byte
                                    char2 -> High Byte
                            
                            */

                            if (reg[R_R0] >= MEMORY_MAX) 
                                fault(VmFaultType::UnterminatedString,
                                    "PUTSP string not Null-Terminated",
                                    reg[R_R0]);

                            uint16_t* c = memory + reg[R_R0];
                            while (*c)
                            {
                                unsigned char char1 = (*c) & 0xFF;
                                putc(char1, stdout);
                                unsigned char char2 = (*c) >> 8;
                                if (char2) putc(char2, stdout);
                                ++c;
                            } // end While
                            fflush(stdout);
                        

                        break;
                    } // end Case TRAP_PUTSP 

                    case TRAP_HALT: {

                        puts("HALT");
                        fflush(stdout);
                        running = 0;

                        break;
                    } // end Case TRAP_HALT

                    default : 
                        fault(VmFaultType::InvalidTrap,
                            "Unknown TRAP vector",
                            instr & 0xFF);

                } // end switch Trap

                break;
            } // end Case Trap

            case OP_RES:
            case OP_RTI:
            default: 
                fault(VmFaultType::InvalidOpcode,
                    "Unknown instruction opcode");
                break;

        } // end Switch
        
        restore_input_buffering();
    }

}