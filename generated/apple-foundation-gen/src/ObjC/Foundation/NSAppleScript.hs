{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAppleScript@.
module ObjC.Foundation.NSAppleScript
  ( NSAppleScript
  , IsNSAppleScript(..)
  , initWithContentsOfURL_error
  , initWithSource
  , compileAndReturnError
  , executeAndReturnError
  , executeAppleEvent_error
  , source
  , compiled
  , compileAndReturnErrorSelector
  , compiledSelector
  , executeAndReturnErrorSelector
  , executeAppleEvent_errorSelector
  , initWithContentsOfURL_errorSelector
  , initWithSourceSelector
  , sourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsNSAppleScript nsAppleScript, IsNSURL url, IsNSDictionary errorInfo) => nsAppleScript -> url -> errorInfo -> IO (Id NSAppleScript)
initWithContentsOfURL_error nsAppleScript url errorInfo =
  sendOwnedMessage nsAppleScript initWithContentsOfURL_errorSelector (toNSURL url) (toNSDictionary errorInfo)

-- | @- initWithSource:@
initWithSource :: (IsNSAppleScript nsAppleScript, IsNSString source) => nsAppleScript -> source -> IO (Id NSAppleScript)
initWithSource nsAppleScript source =
  sendOwnedMessage nsAppleScript initWithSourceSelector (toNSString source)

-- | @- compileAndReturnError:@
compileAndReturnError :: (IsNSAppleScript nsAppleScript, IsNSDictionary errorInfo) => nsAppleScript -> errorInfo -> IO Bool
compileAndReturnError nsAppleScript errorInfo =
  sendMessage nsAppleScript compileAndReturnErrorSelector (toNSDictionary errorInfo)

-- | @- executeAndReturnError:@
executeAndReturnError :: (IsNSAppleScript nsAppleScript, IsNSDictionary errorInfo) => nsAppleScript -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAndReturnError nsAppleScript errorInfo =
  sendMessage nsAppleScript executeAndReturnErrorSelector (toNSDictionary errorInfo)

-- | @- executeAppleEvent:error:@
executeAppleEvent_error :: (IsNSAppleScript nsAppleScript, IsNSAppleEventDescriptor event, IsNSDictionary errorInfo) => nsAppleScript -> event -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAppleEvent_error nsAppleScript event errorInfo =
  sendMessage nsAppleScript executeAppleEvent_errorSelector (toNSAppleEventDescriptor event) (toNSDictionary errorInfo)

-- | @- source@
source :: IsNSAppleScript nsAppleScript => nsAppleScript -> IO (Id NSString)
source nsAppleScript =
  sendMessage nsAppleScript sourceSelector

-- | @- compiled@
compiled :: IsNSAppleScript nsAppleScript => nsAppleScript -> IO Bool
compiled nsAppleScript =
  sendMessage nsAppleScript compiledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSDictionary] (Id NSAppleScript)
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id NSString] (Id NSAppleScript)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @compileAndReturnError:@
compileAndReturnErrorSelector :: Selector '[Id NSDictionary] Bool
compileAndReturnErrorSelector = mkSelector "compileAndReturnError:"

-- | @Selector@ for @executeAndReturnError:@
executeAndReturnErrorSelector :: Selector '[Id NSDictionary] (Id NSAppleEventDescriptor)
executeAndReturnErrorSelector = mkSelector "executeAndReturnError:"

-- | @Selector@ for @executeAppleEvent:error:@
executeAppleEvent_errorSelector :: Selector '[Id NSAppleEventDescriptor, Id NSDictionary] (Id NSAppleEventDescriptor)
executeAppleEvent_errorSelector = mkSelector "executeAppleEvent:error:"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id NSString)
sourceSelector = mkSelector "source"

-- | @Selector@ for @compiled@
compiledSelector :: Selector '[] Bool
compiledSelector = mkSelector "compiled"

