/* The following code was generated by JFlex 1.6.1 */

import java_cup.runtime.*;


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.6.1
 * from the specification file <tt>src/Lexer.lex</tt>
 */
class Lexer implements java_cup.runtime.Scanner {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0, 0
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\3\1\2\1\0\1\3\1\1\22\0\1\3\1\60\1\16"+
    "\1\5\2\11\1\65\1\13\1\44\1\45\1\63\1\61\1\52\1\62"+
    "\1\15\1\4\1\14\11\7\1\54\1\53\1\56\1\55\1\57\2\11"+
    "\5\6\1\10\15\6\1\10\6\6\1\50\1\17\1\51\1\64\1\12"+
    "\1\11\1\21\1\40\1\42\1\27\1\30\1\31\1\6\1\35\1\22"+
    "\1\6\1\41\1\24\1\20\1\23\1\36\1\34\1\43\1\32\1\25"+
    "\1\26\1\33\1\6\1\37\3\6\1\46\1\66\1\47\1\11\6\0"+
    "\1\0\u1fa2\0\1\0\1\0\udfd3\0\1\11\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\22\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\1\0\1\1\2\2\1\3\1\2\1\4\1\5\1\6"+
    "\1\1\1\5\1\7\1\1\1\10\17\4\1\11\1\12"+
    "\1\13\1\14\1\15\1\16\1\17\1\20\1\21\1\1"+
    "\1\22\1\23\1\24\1\25\1\26\1\27\1\30\1\31"+
    "\1\32\6\0\1\33\1\0\2\4\1\34\1\35\6\4"+
    "\1\36\1\4\1\37\10\4\1\40\3\4\1\41\1\42"+
    "\1\43\1\44\1\45\1\46\1\47\1\50\1\51\2\0"+
    "\1\5\1\0\1\52\1\33\2\4\1\53\1\4\1\54"+
    "\1\55\3\4\1\56\7\4\1\57\7\4\1\0\1\60"+
    "\1\4\1\61\1\62\1\63\1\4\1\64\1\65\1\66"+
    "\1\4\1\67\2\4\1\70\2\4\1\71\2\4\1\52"+
    "\1\72\1\4\1\73\3\4\1\74\1\75\1\76\1\77"+
    "\1\4\1\100\1\101\1\4\1\102";

  private static int [] zzUnpackAction() {
    int [] result = new int[157];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\67\0\156\0\67\0\245\0\334\0\u0113\0\u014a"+
    "\0\u0113\0\u0181\0\u01b8\0\67\0\u01ef\0\67\0\u0226\0\u025d"+
    "\0\u0294\0\u02cb\0\u0302\0\u0339\0\u0370\0\u03a7\0\u03de\0\u0415"+
    "\0\u044c\0\u0483\0\u04ba\0\u04f1\0\u0528\0\67\0\67\0\67"+
    "\0\67\0\67\0\67\0\67\0\67\0\u055f\0\u0596\0\u05cd"+
    "\0\u0604\0\u063b\0\67\0\u0672\0\67\0\67\0\u06a9\0\u06e0"+
    "\0\u0717\0\u074e\0\u0785\0\u07bc\0\u07f3\0\u01ef\0\67\0\u082a"+
    "\0\u0861\0\u0898\0\u08cf\0\u0113\0\u0906\0\u093d\0\u0974\0\u09ab"+
    "\0\u09e2\0\u0a19\0\u0113\0\u0a50\0\u0113\0\u0a87\0\u0abe\0\u0af5"+
    "\0\u0b2c\0\u0b63\0\u0b9a\0\u0bd1\0\u0c08\0\u0113\0\u0c3f\0\u0c76"+
    "\0\u0cad\0\67\0\67\0\67\0\67\0\67\0\67\0\67"+
    "\0\67\0\67\0\u0ce4\0\u0d1b\0\u07bc\0\u0d52\0\67\0\u01ef"+
    "\0\u0d89\0\u0dc0\0\u0113\0\u0df7\0\u0113\0\u0113\0\u0e2e\0\u0e65"+
    "\0\u0e9c\0\u0113\0\u0ed3\0\u0f0a\0\u0f41\0\u0f78\0\u0faf\0\u0fe6"+
    "\0\u101d\0\u0113\0\u1054\0\u108b\0\u10c2\0\u10f9\0\u1130\0\u1167"+
    "\0\u119e\0\u11d5\0\u0113\0\u120c\0\u0113\0\u0113\0\u0113\0\u1243"+
    "\0\u0113\0\u0113\0\u0113\0\u127a\0\u0113\0\u12b1\0\u12e8\0\u0113"+
    "\0\u131f\0\u1356\0\u0113\0\u138d\0\u13c4\0\u0113\0\u0113\0\u13fb"+
    "\0\u0113\0\u1432\0\u1469\0\u14a0\0\u0113\0\u0113\0\u0113\0\u0113"+
    "\0\u14d7\0\u0113\0\u0113\0\u150e\0\u0113";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[157];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\1\2\1\3\2\4\1\5\1\6\1\7\1\10\1\11"+
    "\2\2\1\12\1\13\1\14\1\15\1\16\1\17\1\20"+
    "\1\21\1\7\1\22\1\23\1\24\1\25\1\26\1\27"+
    "\1\30\1\7\1\31\1\7\1\32\1\33\1\34\1\7"+
    "\1\35\1\7\1\36\1\37\1\40\1\41\1\42\1\43"+
    "\1\44\1\45\1\46\1\47\1\50\1\51\1\52\1\53"+
    "\1\54\1\55\1\56\1\57\1\60\71\0\1\4\71\0"+
    "\1\61\61\0\1\6\1\3\1\4\64\6\6\0\3\7"+
    "\1\0\1\7\1\0\1\7\3\0\24\7\27\0\1\62"+
    "\2\0\1\10\2\0\1\63\1\0\1\10\1\64\55\0"+
    "\62\65\5\0\1\62\5\0\1\63\2\0\1\64\51\0"+
    "\16\66\1\67\1\70\47\66\6\0\3\7\1\0\1\7"+
    "\1\0\1\7\3\0\1\7\1\71\22\7\31\0\3\7"+
    "\1\0\1\7\1\0\1\7\3\0\4\7\1\72\17\7"+
    "\31\0\3\7\1\0\1\7\1\0\1\7\3\0\3\7"+
    "\1\73\5\7\1\74\12\7\31\0\3\7\1\0\1\7"+
    "\1\0\1\7\3\0\16\7\1\75\5\7\31\0\3\7"+
    "\1\0\1\7\1\0\1\7\3\0\10\7\1\76\13\7"+
    "\31\0\3\7\1\0\1\7\1\0\1\7\3\0\7\7"+
    "\1\77\5\7\1\100\1\101\5\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\2\7\1\102\13\7\1\103"+
    "\5\7\31\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\4\7\1\104\17\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\2\7\1\105\1\7\1\106\2\7\1\107"+
    "\3\7\1\110\2\7\1\111\5\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\1\7\1\112\6\7\1\113"+
    "\13\7\31\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\12\7\1\114\3\7\1\115\5\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\7\7\1\116\14\7\31\0"+
    "\3\7\1\0\1\7\1\0\1\7\3\0\15\7\1\117"+
    "\6\7\31\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\12\7\1\120\11\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\15\7\1\121\6\7\77\0\1\122\1\123"+
    "\66\0\1\124\66\0\1\125\66\0\1\126\66\0\1\127"+
    "\70\0\1\130\74\0\1\131\67\0\1\132\5\133\1\134"+
    "\61\133\7\0\1\135\66\0\1\136\4\0\1\136\61\0"+
    "\1\135\4\0\1\135\65\0\1\137\53\0\16\66\1\140"+
    "\1\70\47\66\6\0\3\7\1\0\1\7\1\0\1\7"+
    "\3\0\2\7\1\141\21\7\31\0\3\7\1\0\1\7"+
    "\1\0\1\7\3\0\2\7\1\142\21\7\31\0\3\7"+
    "\1\0\1\7\1\0\1\7\3\0\6\7\1\143\15\7"+
    "\31\0\3\7\1\0\1\7\1\0\1\7\3\0\16\7"+
    "\1\144\5\7\31\0\3\7\1\0\1\7\1\0\1\7"+
    "\3\0\6\7\1\145\14\7\1\146\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\10\7\1\147\13\7\31\0"+
    "\3\7\1\0\1\7\1\0\1\7\3\0\10\7\1\150"+
    "\1\7\1\151\11\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\14\7\1\152\7\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\22\7\1\153\1\7\31\0"+
    "\3\7\1\0\1\7\1\0\1\7\3\0\2\7\1\154"+
    "\2\7\1\155\16\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\16\7\1\156\5\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\10\7\1\157\13\7\31\0"+
    "\3\7\1\0\1\7\1\0\1\7\3\0\3\7\1\160"+
    "\20\7\31\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\12\7\1\161\11\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\6\7\1\162\15\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\1\7\1\163\4\7\1\164"+
    "\15\7\31\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\2\7\1\165\21\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\16\7\1\166\5\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\2\7\1\167\21\7\31\0"+
    "\3\7\1\0\1\7\1\0\1\7\3\0\10\7\1\170"+
    "\13\7\31\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\1\7\1\171\22\7\23\0\5\133\1\172\61\133\4\0"+
    "\1\4\1\134\65\0\1\62\70\0\3\7\1\0\1\7"+
    "\1\0\1\7\3\0\3\7\1\173\20\7\31\0\3\7"+
    "\1\0\1\7\1\0\1\7\3\0\1\7\1\174\22\7"+
    "\31\0\3\7\1\0\1\7\1\0\1\7\3\0\14\7"+
    "\1\175\7\7\31\0\3\7\1\0\1\7\1\0\1\7"+
    "\3\0\11\7\1\176\12\7\31\0\3\7\1\0\1\7"+
    "\1\0\1\7\3\0\3\7\1\177\20\7\31\0\3\7"+
    "\1\0\1\7\1\0\1\7\3\0\10\7\1\200\13\7"+
    "\31\0\3\7\1\0\1\7\1\0\1\7\3\0\6\7"+
    "\1\201\15\7\31\0\3\7\1\0\1\7\1\0\1\7"+
    "\3\0\11\7\1\202\12\7\31\0\3\7\1\0\1\7"+
    "\1\0\1\7\3\0\10\7\1\203\13\7\31\0\3\7"+
    "\1\0\1\7\1\0\1\7\3\0\1\7\1\204\22\7"+
    "\31\0\3\7\1\0\1\7\1\0\1\7\3\0\11\7"+
    "\1\205\12\7\31\0\3\7\1\0\1\7\1\0\1\7"+
    "\3\0\22\7\1\206\1\7\31\0\3\7\1\0\1\7"+
    "\1\0\1\7\3\0\1\7\1\207\22\7\31\0\3\7"+
    "\1\0\1\7\1\0\1\7\3\0\7\7\1\210\14\7"+
    "\31\0\3\7\1\0\1\7\1\0\1\7\3\0\13\7"+
    "\1\211\10\7\31\0\3\7\1\0\1\7\1\0\1\7"+
    "\3\0\3\7\1\212\20\7\31\0\3\7\1\0\1\7"+
    "\1\0\1\7\3\0\4\7\1\213\17\7\31\0\3\7"+
    "\1\0\1\7\1\0\1\7\3\0\4\7\1\214\17\7"+
    "\31\0\3\7\1\0\1\7\1\0\1\7\3\0\1\7"+
    "\1\215\22\7\31\0\3\7\1\0\1\7\1\0\1\7"+
    "\3\0\12\7\1\216\11\7\23\0\4\133\1\4\1\172"+
    "\61\133\6\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\5\7\1\217\16\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\1\7\1\220\22\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\6\7\1\221\15\7\31\0"+
    "\3\7\1\0\1\7\1\0\1\7\3\0\6\7\1\222"+
    "\15\7\31\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\4\7\1\223\17\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\12\7\1\224\11\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\6\7\1\225\15\7\31\0"+
    "\3\7\1\0\1\7\1\0\1\7\3\0\10\7\1\226"+
    "\13\7\31\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\21\7\1\227\2\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\7\7\1\230\14\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\2\7\1\231\21\7\31\0"+
    "\3\7\1\0\1\7\1\0\1\7\3\0\4\7\1\232"+
    "\17\7\31\0\3\7\1\0\1\7\1\0\1\7\3\0"+
    "\3\7\1\233\20\7\31\0\3\7\1\0\1\7\1\0"+
    "\1\7\3\0\16\7\1\234\5\7\31\0\3\7\1\0"+
    "\1\7\1\0\1\7\3\0\3\7\1\235\20\7\23\0";

  private static int [] zzUnpackTrans() {
    int [] result = new int[5445];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unknown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\1\0\1\11\1\1\1\11\7\1\1\11\1\1\1\11"+
    "\17\1\10\11\5\1\1\11\1\1\2\11\2\1\6\0"+
    "\1\11\1\0\31\1\11\11\2\0\1\1\1\0\1\11"+
    "\32\1\1\0\43\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[157];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the input device */
  private java.io.Reader zzReader;

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /** number of newlines encountered up to the start of the matched text */
  private int yyline;

  /** the number of characters up to the start of the matched text */
  private int yychar;

  /**
   * the number of characters from the last newline up to the start of the 
   * matched text
   */
  private int yycolumn;

  /** 
   * zzAtBOL == true <=> the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true <=> the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;
  
  /** 
   * The number of occupied positions in zzBuffer beyond zzEndRead.
   * When a lead/high surrogate has been read from the input stream
   * into the final zzBuffer position, this will have a value of 1;
   * otherwise, it will have a value of 0.
   */
  private int zzFinalHighSurrogate = 0;

  /* user code: */
    private Symbol symbol(int type) {
        return new Symbol(type, yyline, yycolumn);
    }
  
    private Symbol symbol(int type, Object value) {
        return new Symbol(type, yyline, yycolumn, value);
    }


  /**
   * Creates a new scanner
   *
   * @param   in  the java.io.Reader to read input from.
   */
  Lexer(java.io.Reader in) {
    this.zzReader = in;
  }


  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x110000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 186) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }


  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   * 
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {

    /* first: make room (if you can) */
    if (zzStartRead > 0) {
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
      System.arraycopy(zzBuffer, zzStartRead,
                       zzBuffer, 0,
                       zzEndRead-zzStartRead);

      /* translate stored positions */
      zzEndRead-= zzStartRead;
      zzCurrentPos-= zzStartRead;
      zzMarkedPos-= zzStartRead;
      zzStartRead = 0;
    }

    /* is the buffer big enough? */
    if (zzCurrentPos >= zzBuffer.length - zzFinalHighSurrogate) {
      /* if not: blow it up */
      char newBuffer[] = new char[zzBuffer.length*2];
      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
      zzBuffer = newBuffer;
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
    }

    /* fill the buffer with new input */
    int requested = zzBuffer.length - zzEndRead;
    int numRead = zzReader.read(zzBuffer, zzEndRead, requested);

    /* not supposed to occur according to specification of java.io.Reader */
    if (numRead == 0) {
      throw new java.io.IOException("Reader returned 0 characters. See JFlex examples for workaround.");
    }
    if (numRead > 0) {
      zzEndRead += numRead;
      /* If numRead == requested, we might have requested to few chars to
         encode a full Unicode character. We assume that a Reader would
         otherwise never return half characters. */
      if (numRead == requested) {
        if (Character.isHighSurrogate(zzBuffer[zzEndRead - 1])) {
          --zzEndRead;
          zzFinalHighSurrogate = 1;
        }
      }
      /* potentially more input available */
      return false;
    }

    /* numRead < 0 ==> end of stream */
    return true;
  }

    
  /**
   * Closes the input stream.
   */
  public final void yyclose() throws java.io.IOException {
    zzAtEOF = true;            /* indicate end of file */
    zzEndRead = zzStartRead;  /* invalidate buffer    */

    if (zzReader != null)
      zzReader.close();
  }


  /**
   * Resets the scanner to read from a new input stream.
   * Does not close the old reader.
   *
   * All internal variables are reset, the old input stream 
   * <b>cannot</b> be reused (internal buffer is discarded and lost).
   * Lexical state is set to <tt>ZZ_INITIAL</tt>.
   *
   * Internal scan buffer is resized down to its initial length, if it has grown.
   *
   * @param reader   the new input stream 
   */
  public final void yyreset(java.io.Reader reader) {
    zzReader = reader;
    zzAtBOL  = true;
    zzAtEOF  = false;
    zzEOFDone = false;
    zzEndRead = zzStartRead = 0;
    zzCurrentPos = zzMarkedPos = 0;
    zzFinalHighSurrogate = 0;
    yyline = yychar = yycolumn = 0;
    zzLexicalState = YYINITIAL;
    if (zzBuffer.length > ZZ_BUFFERSIZE)
      zzBuffer = new char[ZZ_BUFFERSIZE];
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final String yytext() {
    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
  }


  /**
   * Returns the character at position <tt>pos</tt> from the 
   * matched text. 
   * 
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch. 
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer[zzStartRead+pos];
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of 
   * yypushback(int) and a match-all fallback rule) this method 
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  } 


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Contains user EOF-code, which will be executed exactly once,
   * when the end of file is reached
   */
  private void zzDoEOF() throws java.io.IOException {
    if (!zzEOFDone) {
      zzEOFDone = true;
      yyclose();
    }
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public java_cup.runtime.Symbol next_token() throws java.io.IOException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    char [] zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      boolean zzR = false;
      int zzCh;
      int zzCharCount;
      for (zzCurrentPosL = zzStartRead  ;
           zzCurrentPosL < zzMarkedPosL ;
           zzCurrentPosL += zzCharCount ) {
        zzCh = Character.codePointAt(zzBufferL, zzCurrentPosL, zzMarkedPosL);
        zzCharCount = Character.charCount(zzCh);
        switch (zzCh) {
        case '\u000B':
        case '\u000C':
        case '\u0085':
        case '\u2028':
        case '\u2029':
          yyline++;
          yycolumn = 0;
          zzR = false;
          break;
        case '\r':
          yyline++;
          yycolumn = 0;
          zzR = true;
          break;
        case '\n':
          if (zzR)
            zzR = false;
          else {
            yyline++;
            yycolumn = 0;
          }
          break;
        default:
          zzR = false;
          yycolumn += zzCharCount;
        }
      }

      if (zzR) {
        // peek one character ahead if it is \n (if we have counted one line too much)
        boolean zzPeek;
        if (zzMarkedPosL < zzEndReadL)
          zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        else if (zzAtEOF)
          zzPeek = false;
        else {
          boolean eof = zzRefill();
          zzEndReadL = zzEndRead;
          zzMarkedPosL = zzMarkedPos;
          zzBufferL = zzBuffer;
          if (eof) 
            zzPeek = false;
          else 
            zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        }
        if (zzPeek) yyline--;
      }
      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      zzState = ZZ_LEXSTATE[zzLexicalState];

      // set up zzAction for empty match case:
      int zzAttributes = zzAttrL[zzState];
      if ( (zzAttributes & 1) == 1 ) {
        zzAction = zzState;
      }


      zzForAction: {
        while (true) {
    
          if (zzCurrentPosL < zzEndReadL) {
            zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
            zzCurrentPosL += Character.charCount(zzInput);
          }
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
              zzCurrentPosL += Character.charCount(zzInput);
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
        zzAtEOF = true;
            zzDoEOF();
          { return new java_cup.runtime.Symbol(sym.EOF); }
      }
      else {
        switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
          case 1: 
            { throw new Error("Illegal character <"+ yytext()+">");
            }
          case 67: break;
          case 2: 
            { /* ignore */
            }
          case 68: break;
          case 3: 
            { return symbol(sym.DIV);
            }
          case 69: break;
          case 4: 
            { return symbol(sym.IDENTIFIER);
            }
          case 70: break;
          case 5: 
            { return symbol(sym.NUMBER);
            }
          case 71: break;
          case 6: 
            { return symbol(sym.BOOLEAN);
            }
          case 72: break;
          case 7: 
            { return symbol(sym.DOT);
            }
          case 73: break;
          case 8: 
            { return symbol(sym.DIFFERENCE);
            }
          case 74: break;
          case 9: 
            { return symbol(sym.LPAREN);
            }
          case 75: break;
          case 10: 
            { return symbol(sym.RPAREN);
            }
          case 76: break;
          case 11: 
            { return symbol(sym.LBRACE);
            }
          case 77: break;
          case 12: 
            { return symbol(sym.RBRACE);
            }
          case 78: break;
          case 13: 
            { return symbol(sym.LBRACK);
            }
          case 79: break;
          case 14: 
            { return symbol(sym.RBRACK);
            }
          case 80: break;
          case 15: 
            { return symbol(sym.COMMA);
            }
          case 81: break;
          case 16: 
            { return symbol(sym.SEMI);
            }
          case 82: break;
          case 17: 
            { return symbol(sym.COLON);
            }
          case 83: break;
          case 18: 
            { return symbol(sym.LT);
            }
          case 84: break;
          case 19: 
            { return symbol(sym.GT);
            }
          case 85: break;
          case 20: 
            { return symbol(sym.NOT);
            }
          case 86: break;
          case 21: 
            { return symbol(sym.PLUS);
            }
          case 87: break;
          case 22: 
            { return symbol(sym.MINUS);
            }
          case 88: break;
          case 23: 
            { return symbol(sym.MULT);
            }
          case 89: break;
          case 24: 
            { return symbol(sym.XOR);
            }
          case 90: break;
          case 25: 
            { return symbol(sym.INTERSECTION);
            }
          case 91: break;
          case 26: 
            { return symbol(sym.UNION);
            }
          case 92: break;
          case 27: 
            { return symbol(sym.STRING);
            }
          case 93: break;
          case 28: 
            { return symbol(sym.IN);
            }
          case 94: break;
          case 29: 
            { return symbol(sym.IF);
            }
          case 95: break;
          case 30: 
            { return symbol(sym.DO);
            }
          case 96: break;
          case 31: 
            { return symbol(sym.FI);
            }
          case 97: break;
          case 32: 
            { return symbol(sym.OD);
            }
          case 98: break;
          case 33: 
            { return symbol(sym.CONCAT);
            }
          case 99: break;
          case 34: 
            { return symbol(sym.EQ);
            }
          case 100: break;
          case 35: 
            { return symbol(sym.EQEQ);
            }
          case 101: break;
          case 36: 
            { return symbol(sym.LTEQ);
            }
          case 102: break;
          case 37: 
            { return symbol(sym.GTEQ);
            }
          case 103: break;
          case 38: 
            { return symbol(sym.NOTEQ);
            }
          case 104: break;
          case 39: 
            { return symbol(sym.IMPLY);
            }
          case 105: break;
          case 40: 
            { return symbol(sym.AND);
            }
          case 106: break;
          case 41: 
            { return symbol(sym.OR);
            }
          case 107: break;
          case 42: 
            { return symbol(sym.CHAR);
            }
          case 108: break;
          case 43: 
            { return symbol(sym.INTEGER);
            }
          case 109: break;
          case 44: 
            { return symbol(sym.SET);
            }
          case 110: break;
          case 45: 
            { return symbol(sym.SEQ);
            }
          case 111: break;
          case 46: 
            { return symbol(sym.TOP);
            }
          case 112: break;
          case 47: 
            { return symbol(sym.RATIONAL);
            }
          case 113: break;
          case 48: 
            { return symbol(sym.MAIN);
            }
          case 114: break;
          case 49: 
            { return symbol(sym.LOOP);
            }
          case 115: break;
          case 50: 
            { return symbol(sym.TDEF);
            }
          case 116: break;
          case 51: 
            { return symbol(sym.THEN);
            }
          case 117: break;
          case 52: 
            { return symbol(sym.DICT);
            }
          case 118: break;
          case 53: 
            { return symbol(sym.ELIF);
            }
          case 119: break;
          case 54: 
            { return symbol(sym.ELSE);
            }
          case 120: break;
          case 55: 
            { return symbol(sym.FDEF);
            }
          case 121: break;
          case 56: 
            { return symbol(sym.READ);
            }
          case 122: break;
          case 57: 
            { return symbol(sym.POOL);
            }
          case 123: break;
          case 58: 
            { return symbol(sym.ALIAS);
            }
          case 124: break;
          case 59: 
            { return symbol(sym.FLOAT);
            }
          case 125: break;
          case 60: 
            { return symbol(sym.PRINT);
            }
          case 126: break;
          case 61: 
            { return symbol(sym.WHILE);
            }
          case 127: break;
          case 62: 
            { return symbol(sym.BREAK);
            }
          case 128: break;
          case 63: 
            { return symbol(sym.THREAD);
            }
          case 129: break;
          case 64: 
            { return symbol(sym.FORALL);
            }
          case 130: break;
          case 65: 
            { return symbol(sym.RETURN);
            }
          case 131: break;
          case 66: 
            { return symbol(sym.FUNCTION);
            }
          case 132: break;
          default:
            zzScanError(ZZ_NO_MATCH);
        }
      }
    }
  }


}
