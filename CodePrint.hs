module CodePrint (printCode) where

import GoatAST
import GoatIR

pLabel :: String -> IO ()
pLabel label
  = putStrLn $ "label_" ++ label ++ ":"

showLabel :: String -> String
showLabel label
  = "label_" ++ label

pInstr :: Instr -> IO ()

pInstr (PushSF size) 
  = do
      pIndent
      putStrLn $ "push_stack_frame " ++ (show size)

pInstr (PopSF size)
  = do
      pIndent
      putStrLn $ "pop_stack_frame " ++ (show size)

pInstr (Store s r) 
  = do
      pIndent
      putStrLn $ "store " ++ (show s) ++ ", " ++ (show r)

pInstr (Load r s)
  = do
      pIndent
      putStrLn $ "load " ++ (show r) ++ ", " ++ (show s)

pInstr (LoadAddr r s)
  = do
      pIndent
      putStrLn $ "load_address " ++ (show r) ++ ", " ++ (show s)

pInstr (LoadIndr r1 r2)
  = do
      pIndent
      putStrLn $ "load_indirect " ++ (show r1) ++ ", " ++ (show r2)

pInstr (StoreIndr r1 r2)
  = do
      pIndent
      putStrLn $ "store_indirect " ++ (show r1) ++ ", " ++ (show r2)

pInstr (IntConstI r val)
  = do
      pIndent
      putStrLn $ "int_const " ++ (show r) ++ ", " ++ (show val)

pInstr (RealConstI r val) 
  = do 
      pIndent
      putStrLn $ "real_const " ++ (show r) ++ ", " ++ (show val)

pInstr (StringConstI r str)
  = do 
      pIndent
      putStrLn $ "string_const " ++ (show r) ++ ", \"" ++ str ++ "\""

pInstr (BinopInstr binop r1 r2 r3)
  = do 
      pIndent
      putStrLn $ (binopToStr binop) ++ " " ++ (show r1) ++ ", " ++
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

pInstr (UnopInstr unop r1 r2)
  = do
      pIndent
      putStrLn $ (unopToStr unop) ++ " " ++ (show r1) ++ ", " ++ (show r2)
  where
    unopToStr :: UnopI -> String
    unopToStr NegInt = "neg_int"
    unopToStr NegReal = "neg_real"
    unopToStr NotI = "not"

pInstr (IntToReal r1 r2)
  = do
      pIndent
      putStrLn $ "int_to_real " ++ (show r1) ++ ", " ++ (show r2)

pInstr (BranchOnTrue r label)
  = do
      pIndent
      putStrLn $ "branch_on_true " ++ (show r) ++ ", " ++ (showLabel label) 

pInstr (BranchOnFalse r label)
  = do
      pIndent
      putStrLn $ "branch_on_false " ++ (show r) ++ ", " ++ (showLabel label)

pInstr (BranchUncond label)
  = do
      pIndent
      putStrLn $ "branch_uncond " ++ (showLabel label) 

pInstr (Call label)
  = do
      pIndent
      putStrLn $ "call " ++ label

pInstr (CallBuiltin builtin) 
  = do 
      pIndent
      putStrLn $ "call_builtin " ++ (builtinToStr builtin)
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
  = do 
      pIndent
      putStrLn $ "return"

pInstr Halt
  = do
      pIndent
      putStrLn $ "halt"

pInstr (DebugReg r) 
  = do
      pIndent
      putStrLn $ "debug_reg " ++ (show r)

pInstr (DebugSlot s) 
  = do
      pIndent
      putStrLn $ "debug_slot " ++ (show s)

pInstr DebugStack
  = do
      pIndent
      putStrLn $ "debug_stack"

pInstr (LabelI label)
  = putStrLn $ "label_" ++ label ++ ":"

pIndent :: IO ()
pIndent
  = do
      putStr "    "

pProc :: ProcCode -> IO ()
pProc (ProcCode ident instrs)
  = do
      pLabel ident
      mapM_ pInstr instrs

printCode :: [ProcCode] -> IO ()
printCode procs
  = do
      pIndent
      pInstr (Call "label_main")
      pIndent
      pInstr Halt
      mapM_ pProc procs
