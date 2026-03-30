def generate_cobol_structure_qa():
    """Generate questions about COBOL program structure"""
    return [
        {"text": "Q: What are the four divisions of a COBOL program?\nA: The four divisions of a COBOL program are:\n1. IDENTIFICATION DIVISION - Contains program metadata like program name and author\n2. ENVIRONMENT DIVISION - Describes the hardware and files used\n3. DATA DIVISION - Defines variables and data structures\n4. PROCEDURE DIVISION - Contains the actual program logic and processing steps"},
        {"text": "What goes in the IDENTIFICATION DIVISION?"},
        {"text": "What is the purpose of the ENVIRONMENT DIVISION?"},
        {"text": "Explain the DATA DIVISION in COBOL."},
        {"text": "What happens in the PROCEDURE DIVISION?"},
        {"text": "What is a paragraph in COBOL?"},
        {"text": "What is a section in COBOL?"},
    ]

def generate_cobol_debugging_qa():
    """Generate questions about debugging COBOL"""
    return [
        {"text": "What is a common COBOL error 'Syntax Error - Invalid token'?"},
        {"text": "How do I fix 'Variable not defined' errors?"},
        {"text": "What causes 'Expected end of file' error?"},
        {"text": "How do I debug numeric calculation errors in COBOL?"},
        {"text": "What is a 'File not found' error and how do I fix it?"},
        {"text": "How do I handle end-of-file (EOF) in COBOL?"},
        {"text": "What does 'Invalid reference modification' mean?"},
    ]

def generate_mainframe_concepts_qa():
    """Generate questions about mainframe concepts"""
    return [
        {"text": "What is a dataset in mainframe systems?"},
        {"text": "Explain JCL and DD statements."},
        {"text": "What are return codes and how do I use them?"},
        {"text": "What is VSAM and when should I use it?"},
        {"text": "What is a GDG (Generation Data Group)?"},
        {"text": "What are fixed and variable length records?"},
        {"text": "What is a copybook in COBOL?"},
    ]

def generate_cobol_functions_qa():
    """Generate questions about COBOL functions and operations"""
    return [
        {"text": "How do I perform string operations in COBOL?"},
        {"text": "Explain COMPUTE statement in COBOL."},
        {"text": "How do I use EVALUATE (switch/case) in COBOL?"},
        {"text": "What are intrinsic functions in COBOL?"},
        {"text": "How do I perform loop operations in COBOL?"},
        {"text": "How do I handle file I/O operations?"},
        {"text": "What is the ACCEPT and DISPLAY statement?"},
        {"text": "How do I handle conditions and IF statements?"},
    ]

def generate_cobol_best_practices_qa():
    """Generate questions about COBOL best practices"""
    return [
        {"text": "What are COBOL naming conventions?"},
        {"text": "How do I organize a COBOL program structure?"},
        {"text": "What error handling techniques should I use?"},
        {"text": "Why use paragraph structure in COBOL?"},
        {"text": "How do I document COBOL code effectively?"},
        {"text": "What performance tips should I know for COBOL?"},
    ]

def generate_cobol_advanced_qa():
    """Generate questions about advanced COBOL topics"""
    return [
        {"text": "How do I use tables/arrays in COBOL?"},
        {"text": "What is the CALL statement and how do I use it?"},
        {"text": "How do I use CONTINUE, EXIT, and STOP in COBOL?"},
        {"text": "What is RECORD VARYING and how do I use it?"},
        {"text": "How do I work with different numeric types (COMP, DISPLAY, etc.)?"},
        {"text": "What is LINAGE and how do I use it for printing?"},
    ]

def generate_cobol_code_correction_qa():
    """Generate questions about correcting COBOL code"""
    return [
        {"text": "Fix this COBOL code: MOVE 'VALUE TO MY-VAR (missing closing quote)"},
        {"text": "What is wrong with this code: READ INFILE NOT AT END PROCESS RECORD (missing AT END clause)?"},
        {"text": "Correct this code: PERFORM PROCESS-DATA UNTIL COUNTER > 100 (missing END-PERFORM scope terminator)"},
        {"text": "Identify the error: COMPUTE TOTAL = PRICE * QUANTITY (missing GIVING clause for proper result handling)"},
        {"text": "Fix this code: IF AGE >= 18 DISPLAY 'Adult' ELSE DISPLAY 'Minor' (missing END-IF scope terminator)"},
        {"text": "What is wrong: OPEN INPUT INFILE OUTPUT OUTFILE (incorrect syntax, should use commas)?"},
        {"text": "Correct: VARIABLE-NAME PIC X(10) COMP-3 (COMP-3 not valid for alphanumeric, should be numeric)"},
        {"text": "Fix this: PERFORM VARYING I FROM 1 UNTIL I > 10 ADD 1 TO I (incorrect; BY clause missing)"},
        {"text": "What is the error: 05 EMPLOYEE-NAME PIC X(20 (missing closing parenthesis)?"},
        {"text": "Fix: CALL 'SUBPROG' USING PARAM1 ON EXCEPTION CONTINUE END-CALL (missing exception handling details)"},
    ]

def generate_cobol_code_generation_qa():
    """Generate questions about generating COBOL code"""
    return [
        {"text": "Write a COBOL program structure with all four divisions."},
        {"text": "Generate code to read a file and count the number of records."},
        {"text": "Write COBOL code to validate that a numeric field is positive."},
        {"text": "Create a paragraph that calculates employee gross pay (hours * rate)."},
        {"text": "Write code to find the maximum value in a table of numbers."},
        {"text": "Generate a PERFORM VARYING loop that processes 100 employee records."},
        {"text": "Write code to convert a date string from YYYYMMDD to MM/DD/YYYY format."},
        {"text": "Create a paragraph that opens an input file, reads records, and writes to output file."},
        {"text": "Write a COBOL IF statement with nested conditions for grade evaluation."},
        {"text": "Generate code using STRING to concatenate first name and last name with a space."},
    ]

def generate_jcl_code_correction_qa():
    """Generate questions about correcting JCL code"""
    return [
        {"text": "Fix this JCL: //JOB1 JOB (ACCT),NAME (missing required parameters)?"},
        {"text": "What is wrong: //STEP1 EXEC PGM=MYPROG PARM=(PARM1,PARM2 (missing closing parenthesis)?"},
        {"text": "Correct this: //INPUT DD DSN=MY.DATA,DISP=SHR (needs full allocation details)?"},
        {"text": "Fix: //OUTPUT DD DSN=MY.OUTPUT.FILE,DISP=(NEW,KEEP SPACE=(CYL,(10,5)) (syntax issues)?"},
        {"text": "What is wrong: //STEP1 EXEC PGM=PROG1 //STEP2 EXEC PGM=PROG2 (STEP2 not properly separated)?"},
        {"text": "Correct: //DD1 DD DSN=DATASET,DISP=(OLD OLD) (incorrect DISP syntax)?"},
        {"text": "Fix: //PROC EXEC MYPROCEDURE PARM1=VALUE (missing proper parameter syntax)?"},
        {"text": "Identify error: //IF (RC > 4) THEN (incorrect JCL conditional syntax)?"},
        {"text": "What is wrong: //JOBSTEP EXEC PGM=MYPROG COND=(4,LT) (incorrect COND syntax)?"},
        {"text": "Fix: //SYSOUT DD CLASS=A (incomplete SYSOUT definition for proper output routing)?"},
    ]

def generate_jcl_code_generation_qa():
    """Generate questions about generating JCL code"""
    return [
        {"text": "Write a complete JCL job to execute a COBOL program named PAYROLL."},
        {"text": "Generate JCL to read input file and write processed output to a new file."},
        {"text": "Create a JCL procedure that runs three programs in sequence."},
        {"text": "Write JCL with conditional logic: if STEP1 returns 0, run STEP2; otherwise skip."},
        {"text": "Generate JCL to allocate a new GDG dataset with proper disposition."},
        {"text": "Write JCL that passes parameters to a COBOL program via PARM clause."},
        {"text": "Create JCL for a job that reads from VSAM file and writes to sequential file."},
        {"text": "Generate JCL to define a temporary dataset for intermediate processing."},
        {"text": "Write JCL for a cataloged procedure that processes payroll data."},
        {"text": "Create JCL that chains multiple jobs with step restart logic."},
    ]

def generate_jcl_qa():
    """Generate questions about JCL (Job Control Language)"""
    return [
        {"text": "What is JCL and what is it used for?"},
        {"text": "Explain the basic structure of a JCL job."},
        {"text": "What does the JOB statement define?"},
        {"text": "What is the EXEC statement and what are its parameters?"},
        {"text": "What are the different types of DD statements in JCL?"},
        {"text": "How do I specify dataset location and allocation in JCL?"},
        {"text": "What are JCL procedures and how do I use them?"},
        {"text": "How do I use conditional execution (COND) in JCL?"},
        {"text": "What is the DISP parameter and what are its options?"},
        {"text": "How do I pass parameters to programs using JCL?"},
        {"text": "What is the difference between IN-STREAM and CATALOGED procedures?"},
        {"text": "How do I handle job failure and return codes in JCL?"},
        {"text": "Is there a way to make Job A wait for Job B to finish before continuing?"},
        {"text": "How do I give my job more memory or space to run a job?"},
    ]

def generate_combined_training_samples():
    """Combine all Q&A categories into training samples"""
    all_samples = []
    
    all_samples.extend(generate_cobol_structure_qa())
    all_samples.extend(generate_cobol_debugging_qa())
    all_samples.extend(generate_mainframe_concepts_qa())
    all_samples.extend(generate_jcl_qa())
    all_samples.extend(generate_cobol_functions_qa())
    all_samples.extend(generate_cobol_best_practices_qa())
    all_samples.extend(generate_cobol_advanced_qa())
    all_samples.extend(generate_cobol_code_correction_qa())
    all_samples.extend(generate_cobol_code_generation_qa())
    all_samples.extend(generate_jcl_code_correction_qa())
    all_samples.extend(generate_jcl_code_generation_qa())
    
    return all_samples


if __name__ == "__main__":
    # Generate and display sample training data
    samples = generate_combined_training_samples()
    print(f"Generated {len(samples)} questions\n")
    
    # Show a few examples
    for i, sample in enumerate(samples[:5], 1):
        print(f"Question {i}: {sample['text']}\n")
