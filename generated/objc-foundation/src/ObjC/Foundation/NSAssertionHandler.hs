{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAssertionHandler@.
module ObjC.Foundation.NSAssertionHandler
  ( NSAssertionHandler
  , IsNSAssertionHandler(..)
  , handleFailureInMethod_object_file_lineNumber_description
  , handleFailureInFunction_file_lineNumber_description
  , currentHandler
  , handleFailureInMethod_object_file_lineNumber_descriptionSelector
  , handleFailureInFunction_file_lineNumber_descriptionSelector
  , currentHandlerSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- handleFailureInMethod:object:file:lineNumber:description:@
handleFailureInMethod_object_file_lineNumber_description :: (IsNSAssertionHandler nsAssertionHandler, IsNSString fileName, IsNSString format) => nsAssertionHandler -> Selector -> RawId -> fileName -> CLong -> format -> IO ()
handleFailureInMethod_object_file_lineNumber_description nsAssertionHandler  selector object fileName line format =
withObjCPtr fileName $ \raw_fileName ->
  withObjCPtr format $ \raw_format ->
      sendMsg nsAssertionHandler (mkSelector "handleFailureInMethod:object:file:lineNumber:description:") retVoid [argPtr (unSelector selector), argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_fileName :: Ptr ()), argCLong (fromIntegral line), argPtr (castPtr raw_format :: Ptr ())]

-- | @- handleFailureInFunction:file:lineNumber:description:@
handleFailureInFunction_file_lineNumber_description :: (IsNSAssertionHandler nsAssertionHandler, IsNSString functionName, IsNSString fileName, IsNSString format) => nsAssertionHandler -> functionName -> fileName -> CLong -> format -> IO ()
handleFailureInFunction_file_lineNumber_description nsAssertionHandler  functionName fileName line format =
withObjCPtr functionName $ \raw_functionName ->
  withObjCPtr fileName $ \raw_fileName ->
    withObjCPtr format $ \raw_format ->
        sendMsg nsAssertionHandler (mkSelector "handleFailureInFunction:file:lineNumber:description:") retVoid [argPtr (castPtr raw_functionName :: Ptr ()), argPtr (castPtr raw_fileName :: Ptr ()), argCLong (fromIntegral line), argPtr (castPtr raw_format :: Ptr ())]

-- | @+ currentHandler@
currentHandler :: IO (Id NSAssertionHandler)
currentHandler  =
  do
    cls' <- getRequiredClass "NSAssertionHandler"
    sendClassMsg cls' (mkSelector "currentHandler") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @handleFailureInMethod:object:file:lineNumber:description:@
handleFailureInMethod_object_file_lineNumber_descriptionSelector :: Selector
handleFailureInMethod_object_file_lineNumber_descriptionSelector = mkSelector "handleFailureInMethod:object:file:lineNumber:description:"

-- | @Selector@ for @handleFailureInFunction:file:lineNumber:description:@
handleFailureInFunction_file_lineNumber_descriptionSelector :: Selector
handleFailureInFunction_file_lineNumber_descriptionSelector = mkSelector "handleFailureInFunction:file:lineNumber:description:"

-- | @Selector@ for @currentHandler@
currentHandlerSelector :: Selector
currentHandlerSelector = mkSelector "currentHandler"

