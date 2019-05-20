module CodePrint (printCode) where

import GoatAST
import GoatIR

pLabel :: String -> IO ()
pLabel label
  = putStrLn $ "label_" ++ label ++ ":"

pInstr :: Instr -> IO ()

pInstr (PushSF size) 
  = putStrLn $ "push_stack_frame " ++ (show size)

pInstr (PopSF size)
  = putStrLn $ "pop_stack_frame " ++ (show size)

pInstr (Store s r) 
  = putStrLn $ "store " ++ (show s) ++ ", " ++ (show r)

pInstr (Load r s)
  = putStrLn $ "load " ++ (show r) ++ ", " ++ (show s)

pInstr (LoadAddr r s)
  = putStrLn $ "load_address " ++ (show r) ++ ", " ++ (show s)

pInstr (LoadIndr r1 r2)
  = putStrLn $ "load_indirect " ++ (show r1) ++ ", " ++ (show r2)

pInstr (StoreIndr r1 r2)
  = putStrLn $ "store_indirect " ++ (show r1) ++ ", " ++ (show r2)

pInstr (IntConstI r val)
  = putStrLn $ "int_const " ++ (show r) ++ ", " ++ (show val)

pInstr (RealConstI r val) 
  = putStrLn $ "real_const " ++ (show r) ++ ", " ++ (show val)

pInstr (StringConstI r str)
  = putStrLn $ "string_const " ++ (show r) ++ ", \"" ++ str ++ "\""

pInstr (BinopInstr binop r1 r2 r3)
  = putStrLn $ (binopToStr binop) ++ " " ++ (show r1) ++ ", " ++
               (show r2) ++ ", " ++ (show r3)
  where
    binopToStr :: BinopI -> String
    binopToStr AddInt = "add_int"
    binopToStr AddReal = "add_real"
    binopToStr AddOff = "add_off"
    binopToStr SubInt = "sub_int"
    binopToStr SubReal = "sub_real"
    binopToStr SubOff = "sub_off"
    binopToStr MulInt = "mul_int"
    binopToStr MulReal = "mul_real"
    binopToStr DivInt = "div_int"
    binopToStr DivReal = "div_real"
    binopToStr EqInt = "cmp_eq_int"
    binopToStr NeInt = "cmp_ne_int"
    binopToStr GtInt = "cmp_gt_int"
    binopToStr GeInt = "cmp_ge_int"
    binopToStr LtInt = "cmp_lt_int"
    binopToStr LeInt = "cmp_le_int"
    binopToStr EqReal = "cmp_eq_real"
    binopToStr NeReal = "cmp_ne_real"
    binopToStr GtReal = "cmp_gt_real"
    binopToStr GeReal = "cmp_ge_real"
    binopToStr LtReal = "cmp_lt_real"
    binopToStr LeReal = "cmp_le_real"
    binopToStr AndI = "and"
    binopToStr OrI = "or"

pInstr (Call label)
  = putStrLn $ "call " ++ label

pInstr (CallBuiltin builtin) 
  = putStrLn $ "call_builtin " ++ (builtinToStr builtin)
  where
    builtinToStr :: Builtin -> String
    builtinToStr ReadInt     = "read_int"
    builtinToStr ReadReal    = "read_real"
    builtinToStr ReadBool    = "read_bool"
    builtinToStr PrintInt    = "print_int"
    builtinToStr PrintReal   = "print_real"
    builtinToStr PrintBool   = "print_bool"
    builtinToStr PrintString = "print_string"

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

printCode :: [ProcCode] -> IO ()
printCode procs
  = do
      pInstrIndent (Call "label_main")
      pInstrIndent Halt
      mapM_ pProc procs
