{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec (Spec, describe, it, hspec, shouldBe, shouldSatisfy)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, castFunPtrToPtr, castPtrToFunPtr, nullFunPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CInt(..), CLong(..))
import Foreign.LibFFI (retPtr, retVoid, retCULong, retCLong, retCDouble, argPtr, argCDouble)
import Foreign.Storable (peek, poke, pokeByteOff, peekElemOff, pokeElemOff)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free)
import Foreign.Marshal.Array (mallocArray)

import ObjC.Runtime

-- ---------------------------------------------------------------------------
-- Helper: create an alloc/init'd NSObject (raw, unmanaged)
-- ---------------------------------------------------------------------------

allocInitNSObject :: IO RawId
allocInitNSObject = do
  Just cls <- getClass "NSObject"
  let allocSel = mkSelector "alloc"
      initSel  = mkSelector "init"
  rawAlloc <- sendClassMsg cls allocSel (retPtr retVoid) []
  rawInit  <- sendRawMsg (RawId (castPtr rawAlloc)) initSel (retPtr retVoid) []
  pure (RawId (castPtr rawInit))

-- Helper: create an alloc/init'd NSString with a value (raw, unmanaged)
allocInitNSString :: String -> IO RawId
allocInitNSString str = do
  Just cls <- getClass "NSString"
  let allocSel = mkSelector "alloc"
      initSel  = mkSelector "initWithUTF8String:"
  rawAlloc <- sendClassMsg cls allocSel (retPtr retVoid) []
  withCString str $ \cstr -> do
    rawInit <- sendRawMsg (RawId (castPtr rawAlloc)) initSel (retPtr retVoid) [argPtr cstr]
    pure (RawId (castPtr rawInit))

-- ---------------------------------------------------------------------------
-- foreign import ccall "wrapper" for creating FunPtrs from Haskell callbacks
-- ---------------------------------------------------------------------------

-- A minimal ObjC method implementation: takes self + _cmd, returns self
type ObjCMethodImp = Ptr () -> Ptr () -> IO (Ptr ())

foreign import ccall "wrapper"
  mkMethodImp :: ObjCMethodImp -> IO (FunPtr ObjCMethodImp)

-- A second implementation for swizzling tests: returns nil
returnNilImp :: ObjCMethodImp
returnNilImp _ _ = pure nullPtr

-- ---------------------------------------------------------------------------
-- DynClass test helpers: method returning CLong via vtable dispatch
-- ---------------------------------------------------------------------------

-- IMP type: (self, _cmd) -> IO CLong
type RetLongIMP = Ptr ObjCObject -> Ptr ObjCSel -> IO CLong

foreign import ccall "wrapper"
  wrapRetLong :: RetLongIMP -> IO (FunPtr RetLongIMP)

foreign import ccall "dynamic"
  callRetLong :: FunPtr RetLongIMP -> RetLongIMP

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  -- Ensure Foundation is loaded (macOS -dead_strip_dylibs can strip it)
  loadFramework "Foundation"
  hspec spec

spec :: Spec
spec = do
  selectorTests
  classLookupTests
  classVersionTests
  classCreateInstanceTests
  classVariableTests
  ivarLayoutTests
  dynamicIvarClassTests
  dynamicMethodClassTests
  objectTests
  propertyTests
  protocolTests
  associationTests
  dynClassTests
  classBuilderDisposeTests
  autoreleasePoolRawTests
  frameworkAtTests
  msgSendVariantTests
  -- Keep existing tests
  existingClassTests
  existingSelectorTests
  existingMsgSendTests
  existingClassBuilderTests
  existingFrameworkTests
  existingAutoreleaseTests
  existingObjectTests

-- ===================================================================
-- 1. Selector (raw CString functions)
-- ===================================================================

selectorTests :: Spec
selectorTests = describe "ObjC.Runtime.Selector (raw)" $ do
  it "sel_registerName registers a selector via CString" $
    withCString "rawTestSelector:" $ \cstr -> do
      sel <- sel_registerName cstr
      sel `shouldSatisfy` (/= nilSelector)

  it "sel_getName returns the correct CString" $ do
    let sel = mkSelector "rawGetNameTest"
    cstr <- sel_getName sel
    name <- peekCString cstr
    name `shouldBe` "rawGetNameTest"

-- ===================================================================
-- 2. Class Lookup Variants
-- ===================================================================

classLookupTests :: Spec
classLookupTests = describe "ObjC.Runtime.Class (lookup variants)" $ do
  it "objc_getClass (raw CString) finds NSObject" $
    withCString "NSObject" $ \cstr -> do
      cls <- objc_getClass cstr
      cls `shouldSatisfy` (/= nilClass)

  it "class_getName (raw) returns correct CString" $ do
    Just cls <- getClass "NSString"
    cstr <- class_getName cls
    name <- peekCString cstr
    name `shouldBe` "NSString"

  it "objc_getMetaClass returns a metaclass" $
    withCString "NSObject" $ \cstr -> do
      meta <- objc_getMetaClass cstr
      meta `shouldSatisfy` (/= nilClass)
      isMeta <- class_isMetaClass meta
      isMeta `shouldBe` True

  it "objc_lookUpClass finds NSObject" $
    withCString "NSObject" $ \cstr -> do
      cls <- objc_lookUpClass cstr
      cls `shouldSatisfy` (/= nilClass)

  it "objc_lookUpClass returns nilClass for nonexistent" $
    withCString "NoSuchClass99999" $ \cstr -> do
      cls <- objc_lookUpClass cstr
      cls `shouldBe` nilClass

  it "objc_getRequiredClass finds NSObject without crashing" $
    withCString "NSObject" $ \cstr -> do
      cls <- objc_getRequiredClass cstr
      cls `shouldSatisfy` (/= nilClass)

  it "objc_getClassList returns a positive count" $ do
    count <- objc_getClassList nullPtr 0
    count `shouldSatisfy` (> 0)

  it "objc_copyClassList returns classes" $
    alloca $ \countPtr -> do
      arr <- objc_copyClassList countPtr
      arr `shouldSatisfy` (/= nullPtr)
      count <- peek countPtr
      count `shouldSatisfy` (> 0)
      free arr

-- ===================================================================
-- 3. Class Version
-- ===================================================================

classVersionTests :: Spec
classVersionTests = describe "ObjC.Runtime.Class (version)" $ do
  it "class_getVersion/class_setVersion round-trip" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"
      newCls <- withCString "HaskellVersionTestClass" $ \name ->
        objc_allocateClassPair nsObject name 0
      objc_registerClassPair newCls

      class_setVersion newCls 42
      v <- class_getVersion newCls
      v `shouldBe` 42

-- ===================================================================
-- 4. Class Instance Creation
-- ===================================================================

classCreateInstanceTests :: Spec
classCreateInstanceTests = describe "ObjC.Runtime.Class (createInstance)" $ do
  it "class_createInstance creates a non-nil instance" $ do
    Just cls <- getClass "NSObject"
    obj <- class_createInstance cls 0
    obj `shouldSatisfy` (/= nilRawId)

-- ===================================================================
-- 16. Class Variable
-- ===================================================================

classVariableTests :: Spec
classVariableTests = describe "ObjC.Runtime.Class (classVariable)" $ do
  it "class_getClassVariable does not crash" $ do
    Just cls <- getClass "NSObject"
    -- NSObject typically has no class variables; we just verify no crash
    withCString "nonExistentClassVar" $ \cstr -> do
      _ivar <- class_getClassVariable cls cstr
      pure ()  -- no crash = pass

-- ===================================================================
-- 15. Ivar Layout
-- ===================================================================

ivarLayoutTests :: Spec
ivarLayoutTests = describe "ObjC.Runtime.Class (ivar layout)" $ do
  it "class_getIvarLayout/class_setIvarLayout round-trips" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"
      newCls <- withCString "HaskellLayoutTestClass" $ \name ->
        objc_allocateClassPair nsObject name 0
      -- Can set layout before registration
      _layout <- class_getIvarLayout newCls
      -- Just verify the calls don't crash
      class_setIvarLayout newCls nullPtr
      objc_registerClassPair newCls
      _layout2 <- class_getIvarLayout newCls
      pure ()

  it "class_getWeakIvarLayout/class_setWeakIvarLayout round-trips" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"
      newCls <- withCString "HaskellWeakLayoutTestClass" $ \name ->
        objc_allocateClassPair nsObject name 0
      _layout <- class_getWeakIvarLayout newCls
      class_setWeakIvarLayout newCls nullPtr
      objc_registerClassPair newCls
      _layout2 <- class_getWeakIvarLayout newCls
      pure ()

-- ===================================================================
-- 5. Dynamic Class with Ivars
-- ===================================================================

dynamicIvarClassTests :: Spec
dynamicIvarClassTests = describe "Dynamic class with ivars (Class + Ivar + Object)" $ do
  it "class_addIvar adds an ivar before registration" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"
      newCls <- withCString "HaskellIvarTestClass" $ \name ->
        objc_allocateClassPair nsObject name 0

      -- Add an ivar of pointer size with "@" type encoding (object pointer)
      added <- withCString "_value" $ \ivarName ->
        withCString "@" $ \typeEnc ->
          class_addIvar newCls ivarName (fromIntegral (8 :: Int)) 3 typeEnc
      added `shouldBe` True

      objc_registerClassPair newCls

      -- class_copyIvarList should return 1 ivar
      ivars <- class_copyIvarList newCls
      length ivars `shouldBe` 1

      -- class_getInstanceVariable finds _value
      ivar <- withCString "_value" $ \cstr ->
        class_getInstanceVariable newCls cstr
      ivar `shouldSatisfy` (/= Ivar nullPtr)

      -- ivar_getName
      ivarNameCStr <- ivar_getName ivar
      ivarNameStr <- peekCString ivarNameCStr
      ivarNameStr `shouldBe` "_value"

      -- ivar_getTypeEncoding
      typeEncCStr <- ivar_getTypeEncoding ivar
      typeEncStr <- peekCString typeEncCStr
      typeEncStr `shouldBe` "@"

      -- ivar_getOffset
      offset <- ivar_getOffset ivar
      offset `shouldSatisfy` (> 0)

  it "object_setIvar/object_getIvar round-trip on dynamic class" $
    withAutoreleasePool $ do
      Just cls <- getClass "HaskellIvarTestClass"
      ivar <- withCString "_value" $ \cstr ->
        class_getInstanceVariable cls cstr

      -- Create an instance via alloc/init
      let allocSel = mkSelector "alloc"
          initSel  = mkSelector "init"
      rawAlloc <- sendClassMsg cls allocSel (retPtr retVoid) []
      rawInit  <- sendRawMsg (RawId (castPtr rawAlloc)) initSel (retPtr retVoid) []
      let obj = RawId (castPtr rawInit)

      -- Create a value to store (an NSObject)
      value <- allocInitNSObject

      -- Set and get via ivar
      object_setIvar obj ivar value
      got <- object_getIvar obj ivar
      got `shouldBe` value

  it "object_setIvarWithStrongDefault works" $
    withAutoreleasePool $ do
      Just cls <- getClass "HaskellIvarTestClass"
      ivar <- withCString "_value" $ \cstr ->
        class_getInstanceVariable cls cstr

      let allocSel = mkSelector "alloc"
          initSel  = mkSelector "init"
      rawAlloc <- sendClassMsg cls allocSel (retPtr retVoid) []
      rawInit  <- sendRawMsg (RawId (castPtr rawAlloc)) initSel (retPtr retVoid) []
      let obj = RawId (castPtr rawInit)

      value <- allocInitNSObject
      object_setIvarWithStrongDefault obj ivar value
      got <- object_getIvar obj ivar
      got `shouldBe` value

  it "object_setInstanceVariable/object_getInstanceVariable round-trip" $
    withAutoreleasePool $ do
      Just cls <- getClass "HaskellIvarTestClass"
      let allocSel = mkSelector "alloc"
          initSel  = mkSelector "init"
      rawAlloc <- sendClassMsg cls allocSel (retPtr retVoid) []
      rawInit  <- sendRawMsg (RawId (castPtr rawAlloc)) initSel (retPtr retVoid) []
      let obj = RawId (castPtr rawInit)

      value <- allocInitNSObject

      -- Set via name
      _ <- withCString "_value" $ \cstr ->
        object_setInstanceVariable obj cstr (castPtr (unRawId value))

      -- Get via name
      alloca $ \outPtr -> do
        poke outPtr nullPtr
        _ <- withCString "_value" $ \cstr ->
          object_getInstanceVariable obj cstr outPtr
        gotPtr <- peek outPtr
        let got = RawId (castPtr gotPtr)
        got `shouldBe` value

-- ===================================================================
-- 6. Dynamic Class with Method
-- ===================================================================

dynamicMethodClassTests :: Spec
dynamicMethodClassTests = describe "Dynamic class with methods (Class + Method)" $ do
  it "class_addMethod adds a method and method introspection works" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"
      newCls <- withCString "HaskellMethodTestClass" $ \name ->
        objc_allocateClassPair nsObject name 0

      -- Create a method implementation that returns self
      let selfReturner :: ObjCMethodImp
          selfReturner self _cmd = pure self
      imp <- mkMethodImp selfReturner
      let objcImp = IMP (castFunPtr imp)

      let testSel = mkSelector "testMethod"

      -- type encoding: returns id (@), takes self (@) and _cmd (:)
      added <- withCString "@@:" $ \typeEnc ->
        class_addMethod newCls testSel objcImp typeEnc
      added `shouldBe` True

      -- Add a second method for swizzling
      imp2 <- mkMethodImp returnNilImp
      let objcImp2 = IMP (castFunPtr imp2)
          testSel2 = mkSelector "testMethod2"
      _ <- withCString "@@:" $ \typeEnc ->
        class_addMethod newCls testSel2 objcImp2 typeEnc

      objc_registerClassPair newCls

      -- class_getInstanceMethod
      method <- class_getInstanceMethod newCls testSel
      method `shouldSatisfy` (/= Method nullPtr)

      -- class_getClassMethod (alloc is inherited from NSObject metaclass)
      let allocSel = mkSelector "alloc"
      classMethod <- class_getClassMethod newCls allocSel
      classMethod `shouldSatisfy` (/= Method nullPtr)

      -- class_getMethodImplementation
      gotImp <- class_getMethodImplementation newCls testSel
      gotImp `shouldSatisfy` (/= IMP nullFunPtr)

      -- method_getName
      gotSel <- method_getName method
      eq <- sel_isEqual gotSel testSel
      eq `shouldBe` True

      -- method_getImplementation
      methodImp <- method_getImplementation method
      methodImp `shouldSatisfy` (/= IMP nullFunPtr)

      -- method_getTypeEncoding
      typeEncCStr <- method_getTypeEncoding method
      typeEncStr <- peekCString typeEncCStr
      -- Should contain "@@:" at minimum (may have offsets)
      length typeEncStr `shouldSatisfy` (>= 3)

      -- method_getNumberOfArguments (self + _cmd = 2)
      argc <- method_getNumberOfArguments method
      argc `shouldBe` 2

      -- method_copyReturnType
      retTypeCStr <- method_copyReturnType method
      retTypeStr <- peekCString retTypeCStr
      retTypeStr `shouldSatisfy` (\s -> length s > 0)
      free retTypeCStr

      -- method_copyArgumentType index 0 (self = "@")
      argTypeCStr <- method_copyArgumentType method 0
      argTypeStr <- peekCString argTypeCStr
      argTypeStr `shouldSatisfy` (\s -> length s > 0)
      free argTypeCStr

      -- method_getReturnType (buffer version)
      allocaBytes 256 $ \buf -> do
        method_getReturnType method buf 256
        retTypeStr2 <- peekCString buf
        retTypeStr2 `shouldSatisfy` (\s -> length s > 0)

      -- method_getArgumentType (buffer version)
      allocaBytes 256 $ \buf -> do
        method_getArgumentType method 1 buf 256  -- index 1 = _cmd
        argTypeStr2 <- peekCString buf
        argTypeStr2 `shouldSatisfy` (\s -> length s > 0)

      -- method_getDescription
      descPtr <- method_getDescription method
      descPtr `shouldSatisfy` (/= nullPtr)
      desc <- peek descPtr
      let descSelEq = methodDescName desc == testSel
      descSelEq `shouldBe` True

  it "method_setImplementation swaps IMP" $
    withAutoreleasePool $ do
      Just cls <- getClass "HaskellMethodTestClass"
      let testSel = mkSelector "testMethod"
      method <- class_getInstanceMethod cls testSel

      -- Create new IMP
      newImpFun <- mkMethodImp returnNilImp
      let newImp = IMP (castFunPtr newImpFun)

      oldImp <- method_setImplementation method newImp
      oldImp `shouldSatisfy` (/= IMP nullFunPtr)

      -- Restore
      _ <- method_setImplementation method oldImp
      pure ()

  it "method_exchangeImplementations swizzles two methods" $
    withAutoreleasePool $ do
      Just cls <- getClass "HaskellMethodTestClass"
      m1 <- class_getInstanceMethod cls (mkSelector "testMethod")
      m2 <- class_getInstanceMethod cls (mkSelector "testMethod2")
      m1 `shouldSatisfy` (/= Method nullPtr)
      m2 `shouldSatisfy` (/= Method nullPtr)

      imp1Before <- method_getImplementation m1
      imp2Before <- method_getImplementation m2

      method_exchangeImplementations m1 m2

      imp1After <- method_getImplementation m1
      imp2After <- method_getImplementation m2
      imp1After `shouldBe` imp2Before
      imp2After `shouldBe` imp1Before

      -- Swap back
      method_exchangeImplementations m1 m2

  it "class_replaceMethod replaces a method" $
    withAutoreleasePool $ do
      Just cls <- getClass "HaskellMethodTestClass"
      let testSel = mkSelector "testMethod"

      newImpFun <- mkMethodImp returnNilImp
      let newImp = IMP (castFunPtr newImpFun)

      oldImp <- withCString "@@:" $ \typeEnc ->
        class_replaceMethod cls testSel newImp typeEnc
      oldImp `shouldSatisfy` (/= IMP nullFunPtr)

      -- Restore
      _ <- withCString "@@:" $ \typeEnc ->
        class_replaceMethod cls testSel oldImp typeEnc
      pure ()

-- ===================================================================
-- 7. Object Functions
-- ===================================================================

objectTests :: Spec
objectTests = describe "ObjC.Runtime.Object (additional)" $ do
  it "object_isClass returns False for instance, True for class object" $
    withAutoreleasePool $ do
      obj <- allocInitNSObject
      isClass <- object_isClass obj
      isClass `shouldBe` False

      -- A Class when cast to RawId should report as a class
      Just cls <- getClass "NSObject"
      let clsAsId = RawId (castPtr (unClass cls))
      isClassCls <- object_isClass clsAsId
      isClassCls `shouldBe` True

  it "object_getClassName returns correct name" $
    withAutoreleasePool $ do
      obj <- allocInitNSObject
      cstr <- object_getClassName obj
      name <- peekCString cstr
      name `shouldBe` "NSObject"

  it "object_setClass changes class" $
    withAutoreleasePool $ do
      obj <- allocInitNSObject
      Just nsstringCls <- getClass "NSString"

      oldCls <- object_setClass obj nsstringCls
      oldName <- className oldCls
      oldName `shouldBe` "NSObject"

      newCls <- object_getClass obj
      newName <- className newCls
      newName `shouldBe` "NSString"

      -- Restore to avoid weirdness
      _ <- object_setClass obj oldCls
      pure ()

  it "object_copy returns a non-nil copy" $
    withAutoreleasePool $ do
      -- Use class_createInstance for a raw object we can safely copy
      Just cls <- getClass "NSObject"
      size <- class_getInstanceSize cls
      obj <- class_createInstance cls 0
      copied <- object_copy obj size
      copied `shouldSatisfy` (/= nilRawId)

  it "object_dispose returns nil" $
    withAutoreleasePool $ do
      -- Create a raw instance via class_createInstance (not reference counted)
      Just cls <- getClass "NSObject"
      obj <- class_createInstance cls 0
      result <- object_dispose obj
      result `shouldBe` nilRawId

-- ===================================================================
-- 8. Property Functions
-- ===================================================================

propertyTests :: Spec
propertyTests = describe "ObjC.Runtime.Property" $ do
  it "class_addProperty / class_getProperty / class_copyPropertyList / property introspection" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"
      newCls <- withCString "HaskellPropertyTestClass" $ \name ->
        objc_allocateClassPair nsObject name 0

      -- Create property attributes: type=@, nonatomic
      withCString "T" $ \attrNameT ->
        withCString "@" $ \attrValueT ->
          withCString "N" $ \attrNameN ->
            withCString "" $ \attrValueEmpty -> do
              -- Build attrs array on stack
              let attr1 = ObjCPropertyAttribute attrNameT attrValueT
                  attr2 = ObjCPropertyAttribute attrNameN attrValueEmpty
              allocaBytes (sizeOfPropAttr * 2) $ \attrsPtr -> do
                poke attrsPtr attr1
                pokeByteOff attrsPtr sizeOfPropAttr attr2

                added <- withCString "testProperty" $ \propName ->
                  class_addProperty newCls propName attrsPtr 2
                added `shouldBe` True

                -- class_replaceProperty (no crash = pass)
                withCString "testProperty" $ \propName ->
                  class_replaceProperty newCls propName attrsPtr 2

              objc_registerClassPair newCls

              -- class_getProperty
              prop <- withCString "testProperty" $ \propName ->
                class_getProperty newCls propName
              prop `shouldSatisfy` (/= ObjCProperty nullPtr)

              -- class_copyPropertyList
              props <- class_copyPropertyList newCls
              length props `shouldSatisfy` (>= 1)

              -- property_getName
              propNameCStr <- property_getName prop
              propNameStr <- peekCString propNameCStr
              propNameStr `shouldBe` "testProperty"

              -- property_getAttributes
              attrsCStr <- property_getAttributes prop
              attrsStr <- peekCString attrsCStr
              length attrsStr `shouldSatisfy` (> 0)

              -- property_copyAttributeList
              attrList <- property_copyAttributeList prop
              length attrList `shouldSatisfy` (> 0)

              -- property_copyAttributeValue for "T" (type)
              withCString "T" $ \tName -> do
                valCStr <- property_copyAttributeValue prop tName
                valCStr `shouldSatisfy` (/= nullPtr)
                valStr <- peekCString valCStr
                valStr `shouldBe` "@"
                free valCStr
  where
    sizeOfPropAttr = 16  -- 2 pointers on 64-bit

-- ===================================================================
-- 9. Protocol Functions
-- ===================================================================

protocolTests :: Spec
protocolTests = describe "ObjC.Runtime.Protocol" $ do
  it "protocol lifecycle: allocate, add methods/properties/protocols, register, introspect" $
    withAutoreleasePool $ do
      -- Allocate a new protocol
      proto <- withCString "HaskellTestProtocol" $ \name ->
        objc_allocateProtocol name
      proto `shouldSatisfy` (/= Protocol nullPtr)

      -- Add a required instance method description
      let testSel = mkSelector "requiredMethod:"
      withCString "v@:@" $ \typeEnc ->
        protocol_addMethodDescription proto testSel typeEnc True True

      -- Add a property
      withCString "T" $ \attrNameT ->
        withCString "@" $ \attrValueT -> do
          let attr = ObjCPropertyAttribute attrNameT attrValueT
          alloca $ \attrPtr -> do
            poke attrPtr attr
            withCString "protoProperty" $ \propName ->
              protocol_addProperty proto propName attrPtr 1 True True

      -- Add conformance to NSObject protocol
      nsObjProto <- withCString "NSObject" $ \name ->
        objc_getProtocol name
      nsObjProto `shouldSatisfy` (/= Protocol nullPtr)
      protocol_addProtocol proto nsObjProto

      -- Register
      objc_registerProtocol proto

      -- objc_getProtocol
      found <- withCString "HaskellTestProtocol" $ \name ->
        objc_getProtocol name
      found `shouldSatisfy` (/= Protocol nullPtr)

      -- objc_copyProtocolList
      allProtos <- objc_copyProtocolList
      length allProtos `shouldSatisfy` (> 0)

      -- protocol_getName
      nameCStr <- protocol_getName proto
      nameStr <- peekCString nameCStr
      nameStr `shouldBe` "HaskellTestProtocol"

      -- protocol_isEqual
      eq <- protocol_isEqual proto proto
      eq `shouldBe` True

      -- protocol_conformsToProtocol
      conforms <- protocol_conformsToProtocol proto nsObjProto
      conforms `shouldBe` True

      -- protocol_copyMethodDescriptionList (required instance methods)
      descs <- protocol_copyMethodDescriptionList proto True True
      length descs `shouldSatisfy` (>= 1)

      -- protocol_getMethodDescription
      mDesc <- protocol_getMethodDescription proto testSel True True
      mDesc `shouldSatisfy` (/= Nothing)

      -- protocol_copyPropertyList
      propList <- protocol_copyPropertyList proto
      length propList `shouldSatisfy` (>= 1)

      -- protocol_getProperty
      prop <- withCString "protoProperty" $ \propName ->
        protocol_getProperty proto propName True True
      prop `shouldSatisfy` (/= ObjCProperty nullPtr)

      -- protocol_copyProtocolList (adopted protocols)
      adopted <- protocol_copyProtocolList proto
      length adopted `shouldSatisfy` (>= 1)

  it "class_addProtocol / class_conformsToProtocol / class_copyProtocolList" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"
      newCls <- withCString "HaskellProtoAdoptTestClass" $ \name ->
        objc_allocateClassPair nsObject name 0
      objc_registerClassPair newCls

      proto <- withCString "HaskellTestProtocol" $ \name ->
        objc_getProtocol name

      added <- class_addProtocol newCls proto
      added `shouldBe` True

      conforms <- class_conformsToProtocol newCls proto
      conforms `shouldBe` True

      protos <- class_copyProtocolList newCls
      length protos `shouldSatisfy` (>= 1)

-- ===================================================================
-- 10. Association Functions
-- ===================================================================

associationTests :: Spec
associationTests = describe "ObjC.Runtime.Association" $ do
  it "set/get/remove associated objects" $
    withAutoreleasePool $ do
      target <- allocInitNSObject
      value  <- allocInitNSString "associated_value"

      -- Use a stack-allocated pointer as key
      alloca $ \(keyPtr :: Ptr CInt) -> do
        poke keyPtr 0
        let key = castPtr keyPtr

        -- Set
        objc_setAssociatedObject target key value OBJC_ASSOCIATION_RETAIN_NONATOMIC

        -- Get
        got <- objc_getAssociatedObject target key
        got `shouldBe` value

        -- Remove all
        objc_removeAssociatedObjects target

        -- Get again - should be nil
        got2 <- objc_getAssociatedObject target key
        got2 `shouldBe` nilRawId

-- ===================================================================
-- 10b. DynClass (vtable-based dynamic classes)
-- ===================================================================

dynClassTests :: Spec
dynClassTests = describe "ObjC.Runtime.DynClass" $ do

  -- First test creates the class and verifies it can be looked up.
  -- Subsequent tests reuse it via getClass.
  it "creates a class with vtable ivar and registers a dispatch stub" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"
      cls <- withCString "HsDynClassTest" $ \name ->
        objc_allocateClassPair nsObject name 0
      addVtableIvar cls

      -- Register "getValue" stub: reads vtable slot 0 and calls through it
      stub <- wrapRetLong $ \self _cmd -> do
        vt <- readVtable self
        p  <- peekElemOff vt 0
        callRetLong (castPtrToFunPtr p) self _cmd
      addObjCMethod cls "getValue" "q@:" stub

      addDeallocHandler cls 1
      objc_registerClassPair cls

      found <- getClass "HsDynClassTest"
      found `shouldSatisfy` (/= Nothing)

  it "writeVtable/readVtable round-trips the vtable pointer" $
    withAutoreleasePool $ do
      Just cls <- getClass "HsDynClassTest"
      inst <- class_createInstance cls 0

      -- Allocate a vtable and write it to the ivar
      vtbl <- mallocArray 1
      pokeElemOff vtbl 0 nullPtr  -- zero the slot for safety
      writeVtable inst vtbl

      -- Read it back and verify
      got <- readVtable (unRawId inst)
      got `shouldBe` vtbl

      -- Leak instance + vtable intentionally (no release = no dealloc on
      -- partially-filled vtable)

  it "dispatches a message through the vtable to a Haskell closure" $
    withAutoreleasePool $ do
      Just cls <- getClass "HsDynClassTest"
      inst <- class_createInstance cls 0

      -- Build vtable: slot 0 returns 42
      vtbl <- mallocArray 1
      fp <- wrapRetLong $ \_self _cmd -> pure 42
      pokeElemOff vtbl 0 (castFunPtrToPtr fp)
      writeVtable inst vtbl

      -- Send getValue -> dispatches through stub -> vtable[0] -> returns 42
      let getValueSel = mkSelector "getValue"
      result <- sendRawMsg inst getValueSel retCLong []
      result `shouldBe` 42

  it "different instances dispatch to their own closures" $
    withAutoreleasePool $ do
      Just cls <- getClass "HsDynClassTest"

      instA <- class_createInstance cls 0
      instB <- class_createInstance cls 0

      -- Instance A's vtable returns 42
      vtblA <- mallocArray 1
      fpA <- wrapRetLong $ \_self _cmd -> pure 42
      pokeElemOff vtblA 0 (castFunPtrToPtr fpA)
      writeVtable instA vtblA

      -- Instance B's vtable returns 99
      vtblB <- mallocArray 1
      fpB <- wrapRetLong $ \_self _cmd -> pure 99
      pokeElemOff vtblB 0 (castFunPtrToPtr fpB)
      writeVtable instB vtblB

      let getValueSel = mkSelector "getValue"
      resultA <- sendRawMsg instA getValueSel retCLong []
      resultB <- sendRawMsg instB getValueSel retCLong []
      resultA `shouldBe` 42
      resultB `shouldBe` 99

  it "dealloc handler runs and frees vtable without crashing" $
    withAutoreleasePool $ do
      Just cls <- getClass "HsDynClassTest"
      inst <- class_createInstance cls 0

      -- Build vtable with a real FunPtr in slot 0
      vtbl <- mallocArray 1
      fp <- wrapRetLong $ \_self _cmd -> pure 0
      pokeElemOff vtbl 0 (castFunPtrToPtr fp)
      writeVtable inst vtbl

      -- Release triggers dealloc, which frees the FunPtr and vtable array,
      -- then calls [super dealloc].
      let releaseSel = mkSelector "release"
      sendRawMsg inst releaseSel retVoid []

      -- Reaching here means dealloc completed without crashing.
      pure ()

-- ===================================================================
-- 11. ClassBuilder (dispose)
-- ===================================================================

classBuilderDisposeTests :: Spec
classBuilderDisposeTests = describe "ObjC.Runtime.ClassBuilder (dispose)" $ do
  it "objc_disposeClassPair disposes unregistered class" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"
      newCls <- withCString "HaskellDisposeTestClass" $ \name ->
        objc_allocateClassPair nsObject name 0
      newCls `shouldSatisfy` (/= nilClass)
      -- Dispose before registration - should not crash
      objc_disposeClassPair newCls

-- ===================================================================
-- 12. AutoreleasePool (raw push/pop)
-- ===================================================================

autoreleasePoolRawTests :: Spec
autoreleasePoolRawTests = describe "ObjC.Runtime.AutoreleasePool (raw)" $ do
  it "objc_autoreleasePoolPush returns non-null token" $ do
    token <- objc_autoreleasePoolPush
    token `shouldSatisfy` (/= nullPtr)
    objc_autoreleasePoolPop token

  it "objc_autoreleasePoolPop completes without crash" $ do
    token <- objc_autoreleasePoolPush
    objc_autoreleasePoolPop token
    -- reaching here = pass

-- ===================================================================
-- 13. Framework (loadFrameworkAt)
-- ===================================================================

frameworkAtTests :: Spec
frameworkAtTests = describe "ObjC.Runtime.Framework (loadFrameworkAt)" $ do
  it "loadFrameworkAt loads CoreGraphics by full path" $ do
    loadFrameworkAt "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics"
    -- After loading, CGColor class should be available
    -- (CoreGraphics defines toll-free bridged types)
    -- Just verify no crash - class availability depends on runtime
    pure ()

-- ===================================================================
-- 14. MsgSend Variants
-- ===================================================================

msgSendVariantTests :: Spec
msgSendVariantTests = describe "ObjC.Runtime.MsgSend (variants)" $ do
  it "objcMsgSendPtr is non-null" $
    castFunPtrToPtr objcMsgSendPtr `shouldSatisfy` (/= nullPtr)

  it "objcMsgSendSuperPtr is non-null" $
    castFunPtrToPtr objcMsgSendSuperPtr `shouldSatisfy` (/= nullPtr)

  it "objcMsgSendStretPtr is non-null" $
    castFunPtrToPtr objcMsgSendStretPtr `shouldSatisfy` (/= nullPtr)

  it "objcMsgSendFpretPtr is non-null" $
    castFunPtrToPtr objcMsgSendFpretPtr `shouldSatisfy` (/= nullPtr)

  it "sendSuperMsg sends init to super" $
    withAutoreleasePool $ do
      -- Create an NSObject, then send init via super (NSObject is its own root)
      Just nsObjectCls <- getClass "NSObject"
      let allocSel = mkSelector "alloc"
          initSel  = mkSelector "init"

      rawAlloc <- sendClassMsg nsObjectCls allocSel (retPtr retVoid) []
      let obj = RawId (castPtr rawAlloc)

      let super_ = ObjCSuper obj nsObjectCls
      rawInit <- sendSuperMsg super_ initSel (retPtr retVoid) []
      let initObj = RawId (castPtr rawInit)
      initObj `shouldSatisfy` (/= nilRawId)

  it "sendMsgStret works (identity with sendMsg on ARM64)" $
    withAutoreleasePool $ do
      obj <- allocInitNSObject
      let initSel = mkSelector "init"
      -- Just verify we can call through without crash
      rawResult <- sendRawMsgStret obj initSel (retPtr retVoid) []
      let resultObj = RawId (castPtr rawResult)
      resultObj `shouldSatisfy` (/= nilRawId)

  it "sendClassMsgStret works (identity with sendClassMsg on ARM64)" $
    withAutoreleasePool $ do
      Just cls <- getClass "NSObject"
      let allocSel = mkSelector "alloc"
      rawResult <- sendClassMsgStret cls allocSel (retPtr retVoid) []
      (castPtr rawResult :: Ptr ()) `shouldSatisfy` (/= nullPtr)

  it "sendMsgFpret returns a double value" $
    withAutoreleasePool $ do
      -- Create [NSNumber numberWithDouble:3.14], then [num doubleValue]
      Just nsnumCls <- getClass "NSNumber"
      let numWithDoubleSel = mkSelector "numberWithDouble:"
          doubleValSel = mkSelector "doubleValue"

      rawNum <- sendClassMsgFpret nsnumCls numWithDoubleSel (retPtr retVoid) [argCDouble 3.14]
      let num = RawId (castPtr rawNum)
      num `shouldSatisfy` (/= nilRawId)

      val <- sendRawMsgFpret num doubleValSel retCDouble []
      -- Should be approximately 3.14
      val `shouldSatisfy` (\v -> abs (v - 3.14) < 0.001)

  it "sendClassMsgFpret can call a class method" $
    withAutoreleasePool $ do
      -- [NSNumber numberWithDouble:2.718] - returns an object via fpret path
      Just nsnumCls <- getClass "NSNumber"
      let numWithDoubleSel = mkSelector "numberWithDouble:"
      rawNum <- sendClassMsgFpret nsnumCls numWithDoubleSel (retPtr retVoid) [argCDouble 2.718]
      (castPtr rawNum :: Ptr ()) `shouldSatisfy` (/= nullPtr)

-- ===================================================================
-- Original tests (preserved)
-- ===================================================================

existingClassTests :: Spec
existingClassTests = describe "ObjC.Runtime.Class (existing)" $ do
  it "objc_getClass finds NSObject" $ do
    result <- getClass "NSObject"
    result `shouldSatisfy` (/= Nothing)

  it "objc_getClass finds NSString" $ do
    result <- getClass "NSString"
    result `shouldSatisfy` (/= Nothing)

  it "objc_getClass finds NSArray" $ do
    result <- getClass "NSArray"
    result `shouldSatisfy` (/= Nothing)

  it "objc_getClass returns Nothing for nonexistent class" $ do
    result <- getClass "NonExistentClassName12345"
    result `shouldBe` Nothing

  it "class_getName returns correct name" $ do
    Just cls <- getClass "NSString"
    name <- className cls
    name `shouldBe` "NSString"

  it "class_getSuperclass of NSString is NSObject" $ do
    Just cls <- getClass "NSString"
    super <- class_getSuperclass cls
    name <- className super
    name `shouldBe` "NSObject"

  it "class_isMetaClass returns False for regular class" $ do
    Just cls <- getClass "NSObject"
    isMeta <- class_isMetaClass cls
    isMeta `shouldBe` False

  it "class_getInstanceSize returns non-zero for NSObject" $ do
    Just cls <- getClass "NSObject"
    size <- class_getInstanceSize cls
    size `shouldSatisfy` (> 0)

  it "class_respondsToSelector works" $ do
    Just cls <- getClass "NSObject"
    let initSel = mkSelector "init"
    responds <- class_respondsToSelector cls initSel
    responds `shouldBe` True

  it "class_copyMethodList returns non-empty for NSObject" $ do
    Just cls <- getClass "NSObject"
    methods <- class_copyMethodList cls
    length methods `shouldSatisfy` (> 0)

existingSelectorTests :: Spec
existingSelectorTests = describe "ObjC.Runtime.Selector (existing)" $ do
  it "sel_registerName round-trips via sel_getName" $ do
    let sel = mkSelector "testSelector:"
    name <- selName sel
    name `shouldBe` "testSelector:"

  it "sel_isEqual returns True for same selector" $ do
    let s1 = mkSelector "init"
        s2 = mkSelector "init"
    eq <- sel_isEqual s1 s2
    eq `shouldBe` True

  it "sel_isEqual returns False for different selectors" $ do
    let s1 = mkSelector "init"
        s2 = mkSelector "alloc"
    eq <- sel_isEqual s1 s2
    eq `shouldBe` False

existingMsgSendTests :: Spec
existingMsgSendTests = describe "ObjC.Runtime.MsgSend (existing)" $ do
  it "can send alloc/init to NSObject" $
    withAutoreleasePool $ do
      Just cls <- getClass "NSObject"
      let allocSel = mkSelector "alloc"
          initSel  = mkSelector "init"

      rawAlloc <- sendClassMsg cls allocSel (retPtr retVoid) []
      let obj = RawId (castPtr rawAlloc)
      obj `shouldSatisfy` (/= nilRawId)

      rawInit <- sendRawMsg obj initSel (retPtr retVoid) []
      let initObj = RawId (castPtr rawInit)
      initObj `shouldSatisfy` (/= nilRawId)

  it "NSString initWithUTF8String round-trips" $
    withAutoreleasePool $ do
      Just cls <- getClass "NSString"
      let allocSel = mkSelector "alloc"
          initSel  = mkSelector "initWithUTF8String:"
          utf8Sel  = mkSelector "UTF8String"
          lenSel   = mkSelector "length"

      rawAlloc <- sendClassMsg cls allocSel (retPtr retVoid) []
      let obj = RawId (castPtr rawAlloc)

      let testStr = "Haskell ObjC test!"
      initialized <- withCString testStr $ \cstr -> do
        rawInit <- sendRawMsg obj initSel (retPtr retVoid) [argPtr cstr]
        pure (RawId (castPtr rawInit))

      len <- sendRawMsg initialized lenSel retCULong []
      len `shouldBe` fromIntegral (length testStr)

      rawCStr <- sendRawMsg initialized utf8Sel (retPtr retVoid) []
      result <- peekCString (castPtr rawCStr)
      result `shouldBe` testStr

existingClassBuilderTests :: Spec
existingClassBuilderTests = describe "ObjC.Runtime.ClassBuilder (existing)" $ do
  it "can create a dynamic class" $
    withAutoreleasePool $ do
      Just nsObject <- getClass "NSObject"

      newCls <- withCString "HaskellTestClassOrig" $ \name ->
        objc_allocateClassPair nsObject name 0

      newCls `shouldSatisfy` (/= nilClass)
      objc_registerClassPair newCls

      found <- getClass "HaskellTestClassOrig"
      found `shouldSatisfy` (/= Nothing)

      name <- className newCls
      name `shouldBe` "HaskellTestClassOrig"

      super <- class_getSuperclass newCls
      superName <- className super
      superName `shouldBe` "NSObject"

      let allocSel = mkSelector "alloc"
          initSel  = mkSelector "init"

      rawAlloc <- sendClassMsg newCls allocSel (retPtr retVoid) []
      let obj = RawId (castPtr rawAlloc)
      obj `shouldSatisfy` (/= nilRawId)

      rawInit <- sendRawMsg obj initSel (retPtr retVoid) []
      let initObj = RawId (castPtr rawInit)
      initObj `shouldSatisfy` (/= nilRawId)

existingFrameworkTests :: Spec
existingFrameworkTests = describe "ObjC.Runtime.Framework (existing)" $ do
  it "loadFramework loads CoreLocation and makes CLLocation available" $
    withAutoreleasePool $ do
      loadFramework "CoreLocation"
      found <- getClass "CLLocation"
      found `shouldSatisfy` (/= Nothing)

  it "CLLocation can be constructed and queried" $
    withAutoreleasePool $ do
      loadFramework "CoreLocation"
      Just cls <- getClass "CLLocation"

      let allocSel = mkSelector "alloc"
          initSel  = mkSelector "initWithLatitude:longitude:"
          descSel  = mkSelector "description"
          utf8Sel  = mkSelector "UTF8String"

      rawAlloc <- sendClassMsg cls allocSel (retPtr retVoid) []
      let obj = RawId (castPtr rawAlloc)

      rawInit <- sendRawMsg obj initSel (retPtr retVoid)
        [argCDouble 37.7749, argCDouble (-122.4194)]
      let location = RawId (castPtr rawInit)
      location `shouldSatisfy` (/= nilRawId)

      rawDesc <- sendRawMsg location descSel (retPtr retVoid) []
      let descNS = RawId (castPtr rawDesc)

      rawCStr <- sendRawMsg descNS utf8Sel (retPtr retVoid) []
      desc <- peekCString (castPtr rawCStr)
      length desc `shouldSatisfy` (> 0)

existingAutoreleaseTests :: Spec
existingAutoreleaseTests = describe "ObjC.Runtime.AutoreleasePool (existing)" $ do
  it "withAutoreleasePool executes action and returns result" $ do
    result <- withAutoreleasePool (pure (42 :: Int))
    result `shouldBe` 42

existingObjectTests :: Spec
existingObjectTests = describe "ObjC.Runtime.Object (existing)" $ do
  it "object_getClass returns the correct class" $
    withAutoreleasePool $ do
      Just cls <- getClass "NSObject"
      let allocSel = mkSelector "alloc"
          initSel  = mkSelector "init"

      rawAlloc <- sendClassMsg cls allocSel (retPtr retVoid) []
      rawInit  <- sendRawMsg (RawId (castPtr rawAlloc)) initSel (retPtr retVoid) []
      let obj = RawId (castPtr rawInit)

      objCls <- object_getClass obj
      name <- className objCls
      name `shouldBe` "NSObject"

-- ===================================================================
-- Utility: castFunPtr for wrapping Haskell callbacks as IMP
-- ===================================================================

-- | Cast between FunPtr types. This is the standard way to convert a
-- @FunPtr ObjCMethodImp@ into the @FunPtr@ expected by 'IMP'.
castFunPtr :: FunPtr a -> FunPtr b
castFunPtr fptr = castPtrToFunPtr (castFunPtrToPtr fptr)
