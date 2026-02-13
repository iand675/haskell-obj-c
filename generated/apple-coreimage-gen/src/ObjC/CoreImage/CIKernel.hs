{-# LANGUAGE DataKinds #-}
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
  , kernelNamesFromMetalLibraryDataSelector
  , kernelWithFunctionName_fromMetalLibraryData_errorSelector
  , kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_errorSelector
  , kernelWithStringSelector
  , kernelsWithMetalString_errorSelector
  , kernelsWithStringSelector
  , nameSelector
  , setROISelectorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ kernelsWithString:@
kernelsWithString :: IsNSString string => string -> IO (Id NSArray)
kernelsWithString string =
  do
    cls' <- getRequiredClass "CIKernel"
    sendClassMessage cls' kernelsWithStringSelector (toNSString string)

-- | @+ kernelsWithMetalString:error:@
kernelsWithMetalString_error :: (IsNSString source, IsNSError error_) => source -> error_ -> IO (Id NSArray)
kernelsWithMetalString_error source error_ =
  do
    cls' <- getRequiredClass "CIKernel"
    sendClassMessage cls' kernelsWithMetalString_errorSelector (toNSString source) (toNSError error_)

-- | @+ kernelWithString:@
kernelWithString :: IsNSString string => string -> IO (Id CIKernel)
kernelWithString string =
  do
    cls' <- getRequiredClass "CIKernel"
    sendClassMessage cls' kernelWithStringSelector (toNSString string)

-- | @+ kernelWithFunctionName:fromMetalLibraryData:error:@
kernelWithFunctionName_fromMetalLibraryData_error :: (IsNSString name, IsNSData data_, IsNSError error_) => name -> data_ -> error_ -> IO (Id CIKernel)
kernelWithFunctionName_fromMetalLibraryData_error name data_ error_ =
  do
    cls' <- getRequiredClass "CIKernel"
    sendClassMessage cls' kernelWithFunctionName_fromMetalLibraryData_errorSelector (toNSString name) (toNSData data_) (toNSError error_)

-- | @+ kernelWithFunctionName:fromMetalLibraryData:outputPixelFormat:error:@
kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_error :: (IsNSString name, IsNSData data_, IsNSError error_) => name -> data_ -> CInt -> error_ -> IO (Id CIKernel)
kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_error name data_ format error_ =
  do
    cls' <- getRequiredClass "CIKernel"
    sendClassMessage cls' kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_errorSelector (toNSString name) (toNSData data_) format (toNSError error_)

-- | @+ kernelNamesFromMetalLibraryData:@
kernelNamesFromMetalLibraryData :: IsNSData data_ => data_ -> IO (Id NSArray)
kernelNamesFromMetalLibraryData data_ =
  do
    cls' <- getRequiredClass "CIKernel"
    sendClassMessage cls' kernelNamesFromMetalLibraryDataSelector (toNSData data_)

-- | @- setROISelector:@
setROISelector :: IsCIKernel ciKernel => ciKernel -> Sel -> IO ()
setROISelector ciKernel method =
  sendMessage ciKernel setROISelectorSelector method

-- | @- name@
name :: IsCIKernel ciKernel => ciKernel -> IO RawId
name ciKernel =
  sendMessage ciKernel nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kernelsWithString:@
kernelsWithStringSelector :: Selector '[Id NSString] (Id NSArray)
kernelsWithStringSelector = mkSelector "kernelsWithString:"

-- | @Selector@ for @kernelsWithMetalString:error:@
kernelsWithMetalString_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSArray)
kernelsWithMetalString_errorSelector = mkSelector "kernelsWithMetalString:error:"

-- | @Selector@ for @kernelWithString:@
kernelWithStringSelector :: Selector '[Id NSString] (Id CIKernel)
kernelWithStringSelector = mkSelector "kernelWithString:"

-- | @Selector@ for @kernelWithFunctionName:fromMetalLibraryData:error:@
kernelWithFunctionName_fromMetalLibraryData_errorSelector :: Selector '[Id NSString, Id NSData, Id NSError] (Id CIKernel)
kernelWithFunctionName_fromMetalLibraryData_errorSelector = mkSelector "kernelWithFunctionName:fromMetalLibraryData:error:"

-- | @Selector@ for @kernelWithFunctionName:fromMetalLibraryData:outputPixelFormat:error:@
kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_errorSelector :: Selector '[Id NSString, Id NSData, CInt, Id NSError] (Id CIKernel)
kernelWithFunctionName_fromMetalLibraryData_outputPixelFormat_errorSelector = mkSelector "kernelWithFunctionName:fromMetalLibraryData:outputPixelFormat:error:"

-- | @Selector@ for @kernelNamesFromMetalLibraryData:@
kernelNamesFromMetalLibraryDataSelector :: Selector '[Id NSData] (Id NSArray)
kernelNamesFromMetalLibraryDataSelector = mkSelector "kernelNamesFromMetalLibraryData:"

-- | @Selector@ for @setROISelector:@
setROISelectorSelector :: Selector '[Sel] ()
setROISelectorSelector = mkSelector "setROISelector:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] RawId
nameSelector = mkSelector "name"

