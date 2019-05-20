module CodePrint (printCode) where

import GoatAST
import GoatIR

pLabel :: String -> IO ()
pLabel label
  = putStrLn $ "label_" ++ label ++ ":"

builtinToStr :: Builtin -> String
builtinToStr ReadInt     = "read_int"
builtinToStr ReadReal    = "read_real"
builtinToStr ReadBool    = "read_bool"
builtinToStr PrintInt    = "print_int"
builtinToStr PrintReal   = "print_real"
builtinToStr PrintBool   = "print_bool"
builtinToStr PrintString = "print_string"

pInstr :: Instr -> IO ()
pInstr (IntConstI (Reg r) val)
  = putStrLn $ "int_const r" ++ (show r) ++ ", " ++ (show val)
pInstr (RealConstI (Reg r) val) 
  = putStrLn $ "real_const r" ++ (show r) ++ ", " ++ (show val)
pInstr (StringConstI (Reg r) str)
  = putStrLn $ "string_const r" ++ (show r) ++ ", \"" ++ str ++ "\""
pInstr (Call label)
  = putStrLn $ "call " ++ label
pInstr (CallBuiltin builtin) 
  = putStrLn $ "call_builtin " ++ (builtinToStr builtin)
pInstr Return
  = putStrLn $ "return"
pInstr Halt
  = putStrLn $ "halt"

pInstrIndent :: Instr -> IO ()
pInstrIndent instr
  = do
      putStr "    "
      pInstr instr

pProc :: ProcCode -> IO ()
pProc (ProcCode ident instrs)
  = do
      pLabel ident
      mapM_ pInstrIndent instrs
      pInstrIndent Return

printCode :: [ProcCode] -> IO ()
printCode procs
  = do
      pInstrIndent (Call "label_main")
      pInstrIndent Halt
      mapM_ pProc procs
