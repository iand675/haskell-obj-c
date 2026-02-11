{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIKernel@.
module ObjC.CoreImage.CIKernel
  ( CIKernel
  , IsCIKernel(..)
  , kernelsWithString
  , kernelsWithMetalString_error
  , kernelWithString
  , kernelWithFunctionName_fromMetalLibraryData_error
  , kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_error
  , kernelNamesFromMetalLibraryData
  , setROISelector
  , name
  , kernelsWithStringSelector
  , kernelsWithMetalString_errorSelector
  , kernelWithStringSelector
  , kernelWithFunctionName_fromMetalLibraryData_errorSelector
  , kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_errorSelector
  , kernelNamesFromMetalLibraryDataSelector
  , setROISelectorSelector
  , nameSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ kernelsWithString:@
kernelsWithString :: IsNSString string => string -> IO (Id NSArray)
kernelsWithString string =
  do
    cls' <- getRequiredClass "CIKernel"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "kernelsWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ kernelsWithMetalString:error:@
kernelsWithMetalString_error :: (IsNSString source, IsNSError error_) => source -> error_ -> IO (Id NSArray)
kernelsWithMetalString_error source error_ =
  do
    cls' <- getRequiredClass "CIKernel"
    withObjCPtr source $ \raw_source ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "kernelsWithMetalString:error:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ kernelWithString:@
kernelWithString :: IsNSString string => string -> IO (Id CIKernel)
kernelWithString string =
  do
    cls' <- getRequiredClass "CIKernel"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "kernelWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ kernelWithFunctionName:fromMetalLibraryData:error:@
kernelWithFunctionName_fromMetalLibraryData_error :: (IsNSString name, IsNSData data_, IsNSError error_) => name -> data_ -> error_ -> IO (Id CIKernel)
kernelWithFunctionName_fromMetalLibraryData_error name data_ error_ =
  do
    cls' <- getRequiredClass "CIKernel"
    withObjCPtr name $ \raw_name ->
      withObjCPtr data_ $ \raw_data_ ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "kernelWithFunctionName:fromMetalLibraryData:error:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ kernelWithFunctionName:fromMetalLibraryData:outputPixelFormat:error:@
kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_error :: (IsNSString name, IsNSData data_, IsNSError error_) => name -> data_ -> CInt -> error_ -> IO (Id CIKernel)
kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_error name data_ format error_ =
  do
    cls' <- getRequiredClass "CIKernel"
    withObjCPtr name $ \raw_name ->
      withObjCPtr data_ $ \raw_data_ ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "kernelWithFunctionName:fromMetalLibraryData:outputPixelFormat:error:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argCInt format, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ kernelNamesFromMetalLibraryData:@
kernelNamesFromMetalLibraryData :: IsNSData data_ => data_ -> IO (Id NSArray)
kernelNamesFromMetalLibraryData data_ =
  do
    cls' <- getRequiredClass "CIKernel"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "kernelNamesFromMetalLibraryData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- setROISelector:@
setROISelector :: IsCIKernel ciKernel => ciKernel -> Selector -> IO ()
setROISelector ciKernel  method =
    sendMsg ciKernel (mkSelector "setROISelector:") retVoid [argPtr (unSelector method)]

-- | @- name@
name :: IsCIKernel ciKernel => ciKernel -> IO RawId
name ciKernel  =
    fmap (RawId . castPtr) $ sendMsg ciKernel (mkSelector "name") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kernelsWithString:@
kernelsWithStringSelector :: Selector
kernelsWithStringSelector = mkSelector "kernelsWithString:"

-- | @Selector@ for @kernelsWithMetalString:error:@
kernelsWithMetalString_errorSelector :: Selector
kernelsWithMetalString_errorSelector = mkSelector "kernelsWithMetalString:error:"

-- | @Selector@ for @kernelWithString:@
kernelWithStringSelector :: Selector
kernelWithStringSelector = mkSelector "kernelWithString:"

-- | @Selector@ for @kernelWithFunctionName:fromMetalLibraryData:error:@
kernelWithFunctionName_fromMetalLibraryData_errorSelector :: Selector
kernelWithFunctionName_fromMetalLibraryData_errorSelector = mkSelector "kernelWithFunctionName:fromMetalLibraryData:error:"

-- | @Selector@ for @kernelWithFunctionName:fromMetalLibraryData:outputPixelFormat:error:@
kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_errorSelector :: Selector
kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_errorSelector = mkSelector "kernelWithFunctionName:fromMetalLibraryData:outputPixelFormat:error:"

-- | @Selector@ for @kernelNamesFromMetalLibraryData:@
kernelNamesFromMetalLibraryDataSelector :: Selector
kernelNamesFromMetalLibraryDataSelector = mkSelector "kernelNamesFromMetalLibraryData:"

-- | @Selector@ for @setROISelector:@
setROISelectorSelector :: Selector
setROISelectorSelector = mkSelector "setROISelector:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

