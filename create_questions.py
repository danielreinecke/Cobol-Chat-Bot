# cobol_error_prompts.py
GENERAL_MAP = {
    # General information about COBOL programming
    "Sequence Number Area": (
        "This area of a line can be blank or contain line sequence numbers."
    ),

    "Indicator Area": (
        "This area has multiple purposes: '*' for comments, ',' for continuation, "
        "'D' or 'd' for debugging lines, and '/' for source listing format."
    ),

    "Area A": (
        "Area A must contain certain items such as Level Indicators, Declaratives, "
        "Division headers, Section headers, Paragraph headers, and Paragraph names."
    ),

    "Area B": (
        "Area B is where you place executable COBOL sentences—this is the main coding area."
    ),

    "Identification Area": (
        "This area is ignored by the compiler, so you may write whatever you like here."
    ),

    "Reserved Words": (
        "These are words with fixed meanings in COBOL and cannot be used for other purposes. "
        "Examples include PERFORM, MOVE, COMPUTE, IF, THEN, ELSE, EVALUATE, and PICTURE."
    ),

    "List of Reserved Words": (
        "+, -, *, /, **, >, <, =, ==, >=, <=, <>, *>, >>, ACCEPT, ACCESS, ACTIVE-CLASS, ADD, "
        "ADDRESS, ADVANCING, AFTER, ALIGNED, ALL, ALLOCATE, ALPHABET, ALPHABETIC, "
        "ALPHABETIC-LOWER, ALPHABETIC-UPPER, ALPHANUMERIC, ALPHANUMERIC-EDITED, ALSO, ALTER, "
        "ALTERNATE, AND, ANY, ANYCASE, APPLY, ARE, AREA, AREAS, ASCENDING, ASSIGN, AT, AUTHOR, "
        "B-AND, B-NOT, B-OR, B-XOR, BASED, BASIS, BEFORE, BEGINNING, BINARY, BINARY-CHAR, "
        "BINARY-DOUBLE, BINARY-LONG, BINARY-SHORT, BIT, BLANK, BLOCK, BOOLEAN, BOTTOM, BY, "
        "BYTE-LENGTH, CALL, CANCEL, CBL, CD, CF, CH, CHARACTER, CHARACTERS, CLASS, CLASS-ID, "
        "CLOCK-UNITS, CLOSE, COBOL, CODE, CODE-SET, COL, COLLATING, COLS, COLUMN, COLUMNS, "
        "COM-REG, COMMA, COMMON, COMMUNICATION, COMP, COMP-1, COMP-2, COMP-3, COMP-4, COMP-5, "
        "COMPUTATIONAL, COMPUTATIONAL-1, COMPUTATIONAL-2, COMPUTATIONAL-3, COMPUTATIONAL-4, "
        "COMPUTATIONAL-5, COMPUTE, CONDITION, CONFIGURATION, CONSTANT, CONTAINS, CONTENT, "
        "CONTINUE, CONTROL, CONTROLS, CONVERTING, COPY, CORR, CORRESPONDING, COUNT, CRT, "
        "CURRENCY, CURSOR, DATA, DATA-POINTER, DATE, DATE-COMPILED, DATE-WRITTEN, DAY, "
        "DAY-OF-WEEK, DBCS, DE, DEBUG-CONTENTS, DEBUG-ITEM, DEBUG-LINE, DEBUG-NAME, "
        "DEBUG-SUB-1, DEBUG-SUB-2, DEBUG-SUB-3, DEBUGGING, DECIMAL-POINT, DECLARATIVES, "
        "DEFAULT, DELETE, DELIMITED, DELIMITER, DEPENDING, DESCENDING, DESTINATION, DETAIL, "
        "DISABLE, DISPLAY, DISPLAY-1, DIVIDE, DIVISION, DOWN, DUPLICATES, DYNAMIC, EC, EGCS, "
        "EGI, EJECT, ELSE, EMI, ENABLE, END, END-ACCEPT, END-ADD, END-CALL, END-COMPUTE, "
        "END-DELETE, END-DISPLAY, END-DIVIDE, END-EVALUATE, END-EXEC, END-IF, END-INVOKE, "
        "END-JSON, END-MULTIPLY, END-OF-PAGE, END-PERFORM, END-READ, END-RECEIVE, END-RETURN, "
        "END-REWRITE, END-SEARCH, END-START, END-STRING, END-SUBTRACT, END-UNSTRING, END-WRITE, "
        "END-XML, ENDING, ENTER, ENTRY, ENVIRONMENT, EO, EOP, EQUAL, ERROR, ESI, EVALUATE, "
        "EVERY, EXCEPTION, EXCEPTION-OBJECT, EXEC, EXECUTE, EXIT, EXTEND, EXTERNAL, FACTORY, "
        "FALSE, FD, FILE, FILE-CONTROL, FILLER, FINAL, FIRST, FLOAT-EXTENDED, FLOAT-LONG, "
        "FLOAT-SHORT, FOOTING, FOR, FORMAT, FREE, FROM, FUNCTION, FUNCTION-ID, FUNCTION-POINTER, "
        "GENERATE, GET, GIVING, GLOBAL, GO, GOBACK, GREATER, GROUP, GROUP-USAGE, HEADING, "
        "HIGH-VALUE, HIGH-VALUES, I-O, I-O-CONTROL, ID, IDENTIFICATION, IF, IN, INDEX, INDEXED, "
        "INDICATE, INHERITS, INITIAL, INITIALIZE, INITIATE, INPUT, INPUT-OUTPUT, INSERT, "
        "INSPECT, INSTALLATION, INTERFACE, INTERFACE-ID, INTO, INVALID, INVOKE, IS, JAVA, "
        "JNIENVPTR, JSON, JSON-CODE, JSON-STATUS, JUST, JUSTIFIED, KANJI, KEY, LABEL, LAST, "
        "LEADING, LEFT, LENGTH, LESS, LIMIT, LIMITS, LINAGE, LINAGE-COUNTER, LINE, "
        "LINE-COUNTER, LINES, LINKAGE, LOCAL-STORAGE, LOCALE, LOCK, LOW-VALUE, LOW-VALUES, "
        "MEMORY, MERGE, MESSAGE, METHOD, METHOD-ID, MINUS, MODE, MODULES, MORE-LABELS, MOVE, "
        "MULTIPLE, MULTIPLY, NATIONAL, NATIONAL-EDITED, NATIVE, NEGATIVE, NESTED, NEXT, NO, "
        "NOT, NULL, NULLS, NUMBER, NUMERIC, NUMERIC-EDITED, OBJECT, OBJECT-COMPUTER, "
        "OBJECT-REFERENCE, OCCURS, OF, OFF, OMITTED, ON, OPEN, OPTIONAL, OPTIONS, OR, ORDER, "
        "ORGANIZATION, OTHER, OUTPUT, OVERFLOW, OVERRIDE, PACKED-DECIMAL, PADDING, PAGE, "
        "PAGE-COUNTER, PASSWORD, PERFORM, PF, PH, PIC, PICTURE, PLUS, POINTER, POINTER-24, "
        "POINTER-31, POINTER-32, POINTER-64, POSITION, POSITIVE, PRESENT, PRINTING, PROCEDURE, "
        "PROCEDURE-POINTER, PROCEDURES, PROCEED, PROCESSING, PROGRAM, PROGRAM-ID, "
        "PROGRAM-POINTER, PROPERTY, PROTOTYPE, PURGE, QUEUE, QUOTE, QUOTES, RAISE, RAISING, "
        "RANDOM, RD, READ, READY, RECEIVE, RECORD, RECORDING, RECORDS, RECURSIVE, REDEFINES, "
        "REEL, REFERENCE, REFERENCES, RELATIVE, RELEASE, RELOAD, REMAINDER, REMOVAL, RENAMES, "
        "REPLACE, REPLACING, REPORT, REPORTING, REPORTS, REPOSITORY, RERUN, RESERVE, RESET, "
        "RESUME, RETRY, RETURN, RETURN-CODE, RETURNING, REVERSED, REWIND, REWRITE, RF, RH, "
        "RIGHT, ROUNDED, RUN, SAME, SCREEN, SD, SEARCH, SECTION, SECURITY, SEGMENT, "
        "SEGMENT-LIMIT, SELECT, SELF, SEND, SENTENCE, SEPARATE, SEQUENCE, SEQUENTIAL, SERVICE, "
        "SET, SHARING, SHIFT-IN, SHIFT-OUT, SIGN, SIZE, SKIP1, SKIP2, SKIP3, SORT, SORT-CONTROL, "
        "SORT-CORE-SIZE, SORT-FILE-SIZE, SORT-MERGE, SORT-MESSAGE, SORT-MODE-SIZE, SORT-RETURN, "
        "SOURCE, SOURCE-COMPUTER, SOURCES, SPACE, SPACES, SPECIAL-NAMES, SQL, SQLIMS, STANDARD, "
        "STANDARD-1, STANDARD-2, START, STATUS, STOP, STRING, SUB-QUEUE-1, SUB-QUEUE-2, "
        "SUB-QUEUE-3, SUBTRACT, SUM, SUPER, SUPPRESS, SYMBOLIC, SYNC, SYNCHRONIZED, "
        "SYSTEM-DEFAULT, TABLE, TALLY, TALLYING, TAPE, TERMINAL, TERMINATE, TEST, TEXT, THAN, "
        "THEN, THROUGH, THRU, TIME, TIMES, TITLE, TO, TOP, TRACE, TRAILING, TRUE, TYPE, "
        "TYPEDEF, UNIT, UNIVERSAL, UNLOCK, UNSTRING, UNTIL, UP, UPON, USAGE, USE, USER-DEFAULT, "
        "USING, UTF-8, VAL-STATUS, VALID, VALIDATE, VALIDATE-STATUS, VALUE, VALUES, VARYING, "
        "VOLATILE, WHEN, WHEN-COMPILED, WITH, WORDS, WORKING-STORAGE, WRITE, WRITE-ONLY, XML, "
        "XML-CODE, XML-EVENT, XML-INFORMATION, XML-NAMESPACE, XML-NAMESPACE-PREFIX, "
        "XML-NNAMESPACE, XML-NNAMESPACE-PREFIX, XML-NTEXT, XML-SCHEMA, XML-TEXT, ZERO, ZEROES, "
        "ZEROS"
    ),

    "Cobol Statements": (
        "These are specific COBOL reserved words used to control execution flow, such as IF "
        "statements or EVALUATE."
    ),

    "Scope terminators": (
        "A scope terminator marks the end of a COBOL construct using the 'END' reserved word. "
        "A period (.) also ends the scope of all statements not otherwise terminated."
    ),

    "Sentence": (
        "A sentence in COBOL is one or more statements followed by the scope terminator '.'"
    ),

    "Paragraph": (
        "A paragraph in COBOL is a user-defined name containing zero or more sentences. "
        "It is similar to a function in other programming languages."
    ),

    "IDENTIFICATION DIVISION": (
        "The only required division in COBOL. It identifies the program by PROGRAM-ID and may "
        "include author, date compiled, and other metadata."
    ),

    "ENVIRONMENT DIVISION": (
        "Describes aspects of the program depending on the computing environment, including "
        "configuration and input/output. This is where device and environment definitions go."
    ),

    "DATA DIVISION": (
        "The DATA DIVISION is split into four sections: FILE SECTION (data for I/O), "
        "LINKAGE SECTION (data passed from other programs), WORKING-STORAGE SECTION "
        "(persistent program data), and LOCAL-STORAGE SECTION (data allocated per call)."
    ),

    "PROCEDURE DIVISION": (
        "The PROCEDURE DIVISION contains all executable code: sentences, paragraphs, and "
        "control structures."
    ),

    "PIC": (
        "PIC (PICTURE clause) defines data types and formats, such as PIC X(9)V99. "
        "V indicates decimal places, X indicates characters, and 9 indicates digits."
    ),

    "DISPLAY": (
        "DISPLAY prints output to the console, similar to a print statement in other languages."
    ),

    "PERFORM": (
        "PERFORM runs a paragraph and can repeat it using UNTIL followed by a condition that "
        "evaluates to TRUE or FALSE."
    ),

    "MOVE": (
        "MOVE copies the value from one field to another; for example, MOVE X TO Y."
    ),

    "ACCEPT": (
        "ACCEPT reads input from the user or system."
    ),

    "OPEN": (
        "OPEN prepares a file for INPUT, OUTPUT, I-O, or EXTEND."
    ),

    "CLOSE": (
        "CLOSE ends file processing and releases it."
    ),

    "COMPUTE": (
        "COMPUTE performs arithmetic operations and stores the result, for example "
        'COMPUTE X = 3 + 1.'
    ),

    "GOBACK": (
        "GOBACK ends the program immediately upon execution."
    )
}

ERROR_MAP = {

    # Classic system ABENDs associated with COBOL logic
    

    "S0C1": (
        "S0C1 is an operation exception. The CPU attempted to execute an "
        "invalid or non-existent instruction. In COBOL this commonly happens "
        "when a program branches to an incorrect address (such as an uninitialized "
        "procedure pointer) or when the load module or called subprogram is "
        "corrupt or mismatched. To resolve it, verify that all CALLed programs "
        "are present and correctly link-edited, ensure that any procedure or "
        "function pointers are initialized before use, and re-link if the load "
        "module may be damaged."
    ),
    "S0C4": (
        "S0C4 is an addressing error. The program attempted to access storage "
        "it does not own or a misaligned address. In COBOL this is often caused "
        "by invalid subscripts or indexes, using reference modification with "
        "out-of-range offsets, or corrupting pointers through incorrect CALL "
        "arguments. To resolve it, use the dump to identify the failing "
        "instruction and data item, check array bounds and reference "
        "modifications, turn on bounds-checking or SSRANGE where possible, "
        "and correct any logic that drives indexes or pointers outside the "
        "allocated storage."
    ),
    "S0C7": (
        "S0C7 is a data exception caused by invalid numeric data in a field that "
        "is used in arithmetic or otherwise treated as numeric. Typical causes "
        "include spaces or non-numeric characters in a packed-decimal (COMP-3) "
        "or zoned-decimal field, or bad group moves overlaying numeric items. "
        "To resolve it, locate the failing instruction and the offending field "
        "in the dump, validate the input data and any group-level MOVEs, ensure "
        "that numeric fields are properly initialized, and add data validation or "
        "edit checks before performing arithmetic."
    ),
    "S0C9": (
        "S0C9 is a divide exception. The program attempted an invalid divide "
        "operation, such as dividing by zero or producing a quotient too large "
        "for the target field. To resolve it, review all DIVIDE statements near "
        "the failing instruction, ensure that divisors are checked for zero or "
        "invalid values, widen result fields if necessary, and add range checks "
        "before performing the division."
    ),
    "S0CB": (
        "S0CB is a decimal overflow exception. A decimal arithmetic operation "
        "produced a result that could not fit into the target packed or zoned "
        "decimal field. To resolve it, examine the arithmetic statement at the "
        "failing instruction, increase the size or precision of the target field, "
        "or add validation and scaling logic so that intermediate and final "
        "results remain within the defined PICTURE ranges."
    ),
    "U4038": (
        "U4038 is a user ABEND frequently issued by Language Environment (LE) "
        "when a COBOL run-time condition is not handled. Typical causes include "
        "unhandled file status errors, invalid arguments on intrinsic functions, "
        "or severe run-time conditions where LE terminates the program. To "
        "resolve it, check the LE message text in the job log (CEE3xxx messages) "
        "for the underlying condition, correct the COBOL logic or data that "
        "caused the error, and optionally enable condition-handling or more "
        "detailed LE diagnostics."
    ),

    #
    # JCL / dataset / allocation related ABENDs
    #
    "S213": (
        "S213 indicates an I/O or dataset access problem. Common causes include "
        "a missing dataset, incorrect data set name, catalog problems, or "
        "insufficient security authorization for the requested dataset. It can "
        "also occur when the dataset is on an unavailable or offline volume. "
        "To resolve it, verify the DD statement's DSN, DISP, and volume "
        "information, ensure that the dataset exists and is cataloged, check "
        "RACF or other security settings for access permissions, and correct "
        "any typos or mismatches between the JCL and the actual dataset."
    ),
    "S013-18": (
        "S013-18 is a dataset open error related to record format or length. "
        "It usually occurs when the program's FD/LRECL/RECFM does not match "
        "the actual dataset attributes, or when the JCL DCB parameters are "
        "inconsistent with the COBOL file description. To resolve it, compare "
        "the dataset's LRECL, BLKSIZE, and RECFM (from LISTCAT or ISPF 3.4) to "
        "the COBOL FD and any DCB parameters in the DD statement, then make "
        "them consistent and rerun the job."
    ),
    "SB37": (
        "SB37 is a space allocation error indicating that a sequential dataset "
        "ran out of primary and secondary extents on the current volume. "
        "This often happens when the output grows larger than anticipated and "
        "no more space can be extended. To resolve it, increase the primary and "
        "secondary SPACE allocation in the DD statement, choose a unit or volume "
        "with more availability, or optimize the program to write less data."
    ),
    "SD37": (
        "SD37 is a space error indicating that a dataset exhausted its primary "
        "space and no secondary space was defined. The system cannot extend the "
        "dataset further. To resolve it, add a nonzero secondary quantity to the "
        "SPACE parameter, increase the primary allocation, or reduce the amount "
        "of data written by the program so that it fits within the allocated "
        "space."
    ),
    "SE37": (
        "SE37 is a space error indicating that a dataset has reached the maximum "
        "number of extents or volumes available. For example, a PDS might reach "
        "its 16-extent limit or a multi-volume sequential dataset may have used "
        "all allocated volumes. To resolve it, increase the SPACE allocation on "
        "fewer but larger volumes, redesign the dataset to require fewer extents "
        "(e.g., use a larger primary and smaller secondary), or split the output "
        "across multiple datasets."
    ),
    "S322": (
        "S322 indicates that the job or step exceeded its CPU or time limit and "
        "was cancelled by the system. This commonly occurs with loops or very "
        "long-running jobs where TIME parameters are too restrictive. To "
        "resolve it, check for infinite or excessive loops in the COBOL program, "
        "optimize expensive processing, and if the logic is correct, increase "
        "the TIME or REGION limits in the JCL as appropriate per site standards."
    ),
    "S522": (
        "S522 indicates that a job was cancelled because it was waiting too long "
        "for an event such as operator intervention or resource availability. "
        "In many shops this arises from jobs waiting at a console prompt or for "
        "a tape, mount, or other resource that never arrives. To resolve it, "
        "verify that the job does not require manual replies, ensure necessary "
        "resources (such as tapes or devices) are available, and consider "
        "removing any code that depends on console I/O in batch."
    ),
    "S806": (
        "S806 indicates that the program could not be found in the STEPLIB, "
        "JOBLIB, or system linklist libraries. The module named in the JCL EXEC "
        "PARM or PROC PGM parameter is not available to be loaded. To resolve it, "
        "verify the program name, confirm that the load module is link-edited "
        "into a library in the appropriate STEPLIB or JOBLIB concatenation, "
        "ensure that the library is cataloged and accessible, and correct any "
        "spelling or concatenation errors in the JCL."
    ),


    # File / I-O behavior frequently seen in COBOL jobs

    "IEC161I": (
        "IEC161I is a message typically associated with end-of-file and file "
        "status issues on QSAM datasets (often paired with ABEND codes). It can "
        "appear when a dataset is not opened correctly or when an unexpected "
        "end-of-file or I/O condition is reached. To resolve it, check the "
        "COBOL file status field after OPEN and READ/WRITE operations, make "
        "sure the DD statement correctly defines the dataset, and ensure that "
        "the program's record format and length match the dataset attributes."
    ),
    "IEC130I": (
        "IEC130I is a dataset attribute error, often indicating an invalid or "
        "inconsistent DCB parameter (such as an illegal BLKSIZE or RECFM). "
        "It can be associated with S013 abends. To resolve it, correct the DCB "
        "parameters on the DD statement, ensure that BLKSIZE is valid for the "
        "device and LRECL, and align the JCL definitions with the actual "
        "dataset and the COBOL FD."
    ),
}

GENERAL_QUESTION_TEMPLATES = [
    "I have a question about {topic}. What does it mean in COBOL?",
    "What is {topic} in COBOL?",
    "Can you explain {topic}?",
    "How is {topic} used in COBOL?",
    "What should I know about {topic} when writing COBOL programs?",
    "How does {topic} work in COBOL?",
    "Why is {topic} important in COBOL?",
    "How do I use {topic} correctly?",
    "I'm confused about {topic}. Can you clarify it?",
    "What are common mistakes people make with {topic}?",
    "Can you give an example that uses {topic}?",
    "In what part of a COBOL program would I use {topic}?",
    "What is the purpose of {topic}?",
    "How would you explain {topic} to a new COBOL programmer?",
]




ERROR_QUESTION_TEMPLATES = [
    "My job ended with error code {code}. What does it mean?",
    "I got abend {code} in my COBOL job. What is this error?",
    "JES is showing {code} for my job. Can you explain what that usually means?",
    "What does error code {code} indicate in a mainframe job?",
    "How do I debug a {code} abend in COBOL?",
    "Why would my job fail with {code}?",
]



def generate_error_samples():

    samples = []

    for code, explanation in ERROR_MAP.items():
        for template in ERROR_QUESTION_TEMPLATES:
            user_question = template.format(code=code)

            sample = (
                "<|user|>\n"
                f"{user_question}\n\n"
                "<|assistant|>\n"
                f"{explanation}\n\n"
            )

            samples.append({"text": sample})

    return samples

def generate_general_samples():
    # This function generate general COBOL topic training questions
    samples = []

    for code, explanation in GENERAL_MAP.items():
        for template in GENERAL_QUESTION_TEMPLATES:
            user_question = template.format(topic=code)

            sample = (
                "<|user|>\n"
                f"{user_question}\n\n"
                "<|assistant|>\n"
                f"{explanation}\n\n"
            )

            samples.append({"text": sample})

    return samples

def generate_combined_training_samples():
    samples = []
    samples.extend(generate_general_samples())
    samples.extend(generate_error_samples())
    return samples