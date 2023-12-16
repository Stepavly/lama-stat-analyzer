/* Lama SM Bytecode interpreter */

# include <string.h>
# include <stdio.h>
# include <errno.h>
# include <malloc.h>
# include <cstdarg>
# include <cstdlib>
# include <unordered_map>
# include <string>
# include "byterun.hpp"
# include <vector>
# include <algorithm>
# include <iostream>

static void vfailure (const char *s, va_list args) {
  fflush   (stdout);
  fprintf  (stderr, "*** FAILURE: ");
  vfprintf (stderr, s, args); // vprintf (char *, va_list) <-> printf (char *, ...)
  exit     (255);
}

void failure (const char *s, ...) {
  va_list args;

  va_start (args, s);
  vfailure (s, args);
}

/* The unpacked representation of bytecode file */
typedef struct {
  char *string_ptr;              /* A pointer to the beginning of the string table */
  int  *public_ptr;              /* A pointer to the beginning of publics table    */
  char *code_ptr;                /* A pointer to the bytecode itself               */
  int  *global_ptr;              /* A pointer to the global area                   */
  int   stringtab_size;          /* The size (in bytes) of the string table        */
  int   global_area_size;        /* The size (in words) of global area             */
  int   public_symbols_number;   /* The number of public symbols                   */
  char  buffer[0];               
} bytefile;

/* Gets a string from a string table by an index */
char* get_string (bytefile *f, int pos) {
  return &f->string_ptr[pos];
}

/* Gets a name for a public symbol */
char* get_public_name (bytefile *f, int i) {
  return get_string (f, f->public_ptr[i*2]);
}

/* Gets an offset for a publie symbol */
int get_public_offset (bytefile *f, int i) {
  return f->public_ptr[i*2+1];
}

enum lama_instr_type {
  UNTYPED
};

struct lama_instr {
  size_t len;
  const char *ptr;

  lama_instr() : len(0), ptr(nullptr) {
  }

  lama_instr(size_t len, const char *ptr) : len(len), ptr(ptr) {
  }


  int hash() const {
    if (ptr == nullptr) {
      return 0;
    }
    int h = 0;
    for (size_t i = 0; i < len; i++) {
      h = h * 31 + ptr[i];
    }
    return h;
  }

  bool operator==(const lama_instr& other) const {
    return len == other.len && memcmp(ptr, other.ptr, len) == 0;
  }

private:
  static std::string to_hex_string(size_t num, size_t width) {
    std::string s(width + 2, '0');
    s[1] = 'x';
    width += 1;
    while (width > 1 && num > 0) {
      size_t rem = num % 16;
      if (rem < 10) {
        s[width] = '0' + rem;
      } else {
        s[width] = 'A' + (rem - 10);
      }
      width--;
      num /= 16;
    } 
    return s;
  }

public:

  std::string to_string(bytefile *bf) const {
    const char *ip = ptr;
# define INT    (ip += sizeof (int), *(int*)(ip - sizeof (int)))
# define BYTE   *ip++
# define STRING get_string (bf, INT)

    static const char *ops [] = {"+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "!!"};
    static const char *pats[] = {"=str", "#string", "#array", "#sexp", "#ref", "#val", "#fun"};
    static const char *lds [] = {"LD", "LDA", "ST"};

    char x = BYTE,
         h = (x & 0xF0) >> 4,
         l = x & 0x0F;
    
    switch (h) {
    case L_EXIT:
      return "STOP";
      
    case L_BINOP:
      return std::string("BINOP ") + ops[l - 1];

    case L_LD:
    case L_LDA:
    case L_ST: {
      std::string res = lds[h - 2];
      res += " ";
      switch (l) {
      case 0: res += "G"; break;
      case 1: res += "L"; break;
      case 2: res += "A"; break;
      case 3: res += "C"; break;
      }
      res += "(";
      res += std::to_string(INT);
      res += ")";
      return res;
    }
      
    case L_PATT:
      switch (l)
      {
      case PATT_STR:
      case PATT_TAG_STR:
      case PATT_TAG_ARR:
      case PATT_TAG_SEXP:
      case PATT_BOXED:
      case PATT_UNBOXED:
      case PATT_TAG_CLOSURE:
        return std::string("PATT ") + pats[l];
      
      default:
        exit(1);
        return "";
      }
      
    default:
      0 == 0;
      // nop
    }

    switch (x)
    {
    case  L_STI:
      return "STI";
      
    case  L_STA:
      return "STA";
        
    case  L_END:
      return "END";
      
    case  L_RET:
      return "RET";
      
    case  L_DROP:
      return "DROP";
      
    case  L_DUP:
      return "DUP";
        
    case L_SWAP:
      return "SWAP";

    case L_ELEM:
      return "ELEM";

    case L_CALL_READ:
      return "CALL Lread";
      
    case L_CALL_WRITE:
      return "CALL Lwrite";

    case L_CALL_LENGTH:
      return "CALL Llength";

    case L_CALL_STRING:
      return "CALL Lstring";

    case L_CALL_ARRAY:
      return std::string("CALL Barray") + std::to_string(INT);

    case L_CONST:
      return std::string("CONST ") + std::to_string(INT);
      
    case L_STRING:
      return std::string("STRING ") + STRING;

    case L_JMP:
      return std::string("JMP ") + to_hex_string(INT, 8);

    case L_CJMP_Z:
      return std::string("CJMPz ") + to_hex_string(INT, 8);
      
    case L_CJMP_NZ:
      return std::string("CJMPnz ") + to_hex_string(INT, 8);
    
          
    case L_CALLC:
      return std::string("CALLC ") + std::to_string(INT);
      
    case L_CALL: {
      std::string res = "CALL ";
      res += to_hex_string(INT, 8);
      res += " ";
      res += std::to_string(INT);
      return res;
    }
        
    case L_ARRAY:
      return std::string("ARRAY ") + std::to_string(INT);
        
    case L_LINE:
      return std::string("LINE ") + std::to_string(INT);

    case L_SEXP: {
      std::string res = "SEXP ";
      res += STRING;
      res += " ";
      res += std::to_string(INT);
      return res;
    }

    case L_FAIL: {
      std::string res = "FAIL ";
      res += to_hex_string(INT, 8);
      res += " ";
      res += std::to_string(INT);
      return res;
    }

    case L_BEGIN: 
    case L_CBEGIN:{
      std::string res;
      if (x == L_BEGIN) {
        res += "BEGIN ";
      } else {
        res += "CBEGIN ";
      }
      res += std::to_string(INT);
      res += ' ';
      res += std::to_string(INT);
      return res;
    }
    
    case L_TAG: {
      std::string res = "TAG ";
      res += STRING;
      res += " ";
      res += std::to_string(INT);
      return res;
    }

    case L_CLOSURE: {
      std::string res = std::string("CLOSURE ") + to_hex_string(INT, 8);
      int n = INT;
      for (int i = 0; i < n; i++) {
        switch (BYTE) {
          case 0: res += 'G'; break;
          case 1: res += 'L'; break;
          case 2: res += 'A'; break;
          case 3: res += 'C'; break;
        }
        res += '(';
        res += std::to_string(INT);
        res += ')';
      }
      return res;
    }

    default:
      exit(1);
      return "";
    }
  }
};

struct lama_instr_hasher {
public:
  int operator()(const lama_instr& instr) const {
    return instr.hash();
  }
};

/* Reads a binary bytecode file by name and unpacks it */
bytefile* read_file (char *fname) {
  FILE *f = fopen (fname, "rb");
  long size;
  bytefile *file;

  if (f == 0) {
    failure ("%s\n", strerror (errno));
  }
  
  if (fseek (f, 0, SEEK_END) == -1) {
    failure ("%s\n", strerror (errno));
  }

  file = (bytefile*) malloc (sizeof(int)*4 + (size = ftell (f)));

  if (file == 0) {
    failure ("*** FAILURE: unable to allocate memory.\n");
  }
  
  rewind (f);

  if (size != fread (&file->stringtab_size, 1, size, f)) {
    failure ("%s\n", strerror (errno));
  }
  
  fclose (f);
  
  file->string_ptr  = &file->buffer [file->public_symbols_number * 2 * sizeof(int)];
  file->public_ptr  = (int*) file->buffer;
  file->code_ptr    = &file->string_ptr [file->stringtab_size];
  file->global_ptr  = (int*) malloc (file->global_area_size * sizeof (int));
  
  return file;
}

/* Disassembles the bytecode pool */
std::unordered_map<lama_instr, size_t, lama_instr_hasher> disassemble (bytefile *bf) {
  
# define INT    (ip += sizeof (int), *(int*)(ip - sizeof (int)))
# define BYTE   *ip++
# define STRING get_string (bf, INT)
# define FAIL   failure ("ERROR: invalid opcode %d-%d\n", h, l)
  
  char *ip     = bf->code_ptr;
  const char *ops [] = {"+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "!!"};
  const char *pats[] = {"=str", "#string", "#array", "#sexp", "#ref", "#val", "#fun"};
  const char *lds [] = {"LD", "LDA", "ST"};
  std::unordered_map<lama_instr, size_t, lama_instr_hasher> statistics;

  while (ip != nullptr) {
    char x = BYTE,
         h = (x & 0xF0) >> 4,
         l = x & 0x0F;
    
    switch (h) {
    case L_EXIT:
    case L_BINOP:
      statistics[lama_instr(1, ip - 1)]++;
      if (h == L_EXIT) {
        ip = nullptr;
      }
      continue;

    case L_LD:
    case L_LDA:
    case L_ST: 
      statistics[lama_instr(5, ip - 1)]++;
      ip += 4;
      continue;
      
    case L_PATT:
      switch (l)
      {
      case PATT_STR:
      case PATT_TAG_STR:
      case PATT_TAG_ARR:
      case PATT_TAG_SEXP:
      case PATT_BOXED:
      case PATT_UNBOXED:
      case PATT_TAG_CLOSURE:
        statistics[lama_instr(1, ip - 1)]++;
        continue;
      
      default:
        failure("Unexpected %d code in PATT command", (int) l);
        continue;
      }
      continue;
      
    default:
      0 == 0;
      // nop
    }

    switch (x)
    {
    case L_STI:
    case L_STA:
    case L_END:
    case L_RET:
    case L_DROP:
    case L_DUP:
    case L_SWAP: 
    case L_ELEM: 
    case L_CALL_READ:
    case L_CALL_WRITE:
    case L_CALL_LENGTH:
    case L_CALL_STRING:
      statistics[lama_instr(1, ip - 1)]++;
      break;

    case L_CONST:
    case L_STRING:
    case L_JMP:
    case L_CJMP_Z: 
    case L_CJMP_NZ: 
    case L_CALLC:
    case L_ARRAY:
    case L_LINE:
    case L_CALL_ARRAY:
      statistics[lama_instr(5, ip - 1)]++;
      ip += 4;
      break;

    case L_SEXP:
    case L_BEGIN:
    case L_CBEGIN:
    case L_CALL:
    case L_TAG:
    case L_FAIL:
      statistics[lama_instr(9, ip - 1)]++;
      ip += 8;
      break;
    case L_CLOSURE: {
      size_t addr = INT;
      size_t argc = INT;
      statistics[lama_instr(9 + 5 * argc, ip - 9)]++;
      ip += 5 * argc;
      break;
    }

    default:
      FAIL;
      break;
    }
  }

  return statistics;
}

int main (int argc, char* argv[]) {
  bytefile *f = read_file (argv[1]);
  std::unordered_map<lama_instr, size_t, lama_instr_hasher> statistics = disassemble(f);
  std::vector<std::pair<lama_instr, size_t>> statistics_entries(statistics.size());
  std::copy(statistics.begin(), statistics.end(), statistics_entries.begin());
  std::sort(statistics_entries.begin(), statistics_entries.end(), [](const auto& entry1, const auto& entry2) {
    return entry1.second > entry2.second;
  });

  std::vector<std::string> entires_str(statistics_entries.size());
  size_t max_str_size = 0;
  for (size_t i = 0; i < statistics_entries.size(); i++) {
    entires_str[i] = std::move(statistics_entries[i].first.to_string(f));
    max_str_size = std::max(max_str_size, entires_str[i].size());
  }

  const size_t alignment = 4;

  for (size_t i = 0; i < statistics_entries.size(); i++) {
    const std::pair<lama_instr, size_t>& entry = statistics_entries[i];
    std::cout << entires_str[i] 
              << std::string(max_str_size + alignment - entires_str[i].size(), ' ') 
              << entry.second 
              << std::endl;
  }
  return 0;
}