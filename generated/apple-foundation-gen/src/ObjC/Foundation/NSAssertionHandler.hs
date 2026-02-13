{-# LANGUAGE DataKinds #-}
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
  , currentHandlerSelector
  , handleFailureInFunction_file_lineNumber_descriptionSelector
  , handleFailureInMethod_object_file_lineNumber_descriptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- handleFailureInMethod:object:file:lineNumber:description:@
handleFailureInMethod_object_file_lineNumber_description :: (IsNSAssertionHandler nsAssertionHandler, IsNSString fileName, IsNSString format) => nsAssertionHandler -> Sel -> RawId -> fileName -> CLong -> format -> IO ()
handleFailureInMethod_object_file_lineNumber_description nsAssertionHandler selector object fileName line format =
  sendMessage nsAssertionHandler handleFailureInMethod_object_file_lineNumber_descriptionSelector selector object (toNSString fileName) line (toNSString format)

-- | @- handleFailureInFunction:file:lineNumber:description:@
handleFailureInFunction_file_lineNumber_description :: (IsNSAssertionHandler nsAssertionHandler, IsNSString functionName, IsNSString fileName, IsNSString format) => nsAssertionHandler -> functionName -> fileName -> CLong -> format -> IO ()
handleFailureInFunction_file_lineNumber_description nsAssertionHandler functionName fileName line format =
  sendMessage nsAssertionHandler handleFailureInFunction_file_lineNumber_descriptionSelector (toNSString functionName) (toNSString fileName) line (toNSString format)

-- | @+ currentHandler@
currentHandler :: IO (Id NSAssertionHandler)
currentHandler  =
  do
    cls' <- getRequiredClass "NSAssertionHandler"
    sendClassMessage cls' currentHandlerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @handleFailureInMethod:object:file:lineNumber:description:@
handleFailureInMethod_object_file_lineNumber_descriptionSelector :: Selector '[Sel, RawId, Id NSString, CLong, Id NSString] ()
handleFailureInMethod_object_file_lineNumber_descriptionSelector = mkSelector "handleFailureInMethod:object:file:lineNumber:description:"

-- | @Selector@ for @handleFailureInFunction:file:lineNumber:description:@
handleFailureInFunction_file_lineNumber_descriptionSelector :: Selector '[Id NSString, Id NSString, CLong, Id NSString] ()
handleFailureInFunction_file_lineNumber_descriptionSelector = mkSelector "handleFailureInFunction:file:lineNumber:description:"

-- | @Selector@ for @currentHandler@
currentHandlerSelector :: Selector '[] (Id NSAssertionHandler)
currentHandlerSelector = mkSelector "currentHandler"

