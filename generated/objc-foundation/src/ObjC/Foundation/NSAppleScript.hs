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
  , initWithContentsOfURL_errorSelector
  , initWithSourceSelector
  , compileAndReturnErrorSelector
  , executeAndReturnErrorSelector
  , executeAppleEvent_errorSelector
  , sourceSelector
  , compiledSelector


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

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsNSAppleScript nsAppleScript, IsNSURL url, IsNSDictionary errorInfo) => nsAppleScript -> url -> errorInfo -> IO (Id NSAppleScript)
initWithContentsOfURL_error nsAppleScript  url errorInfo =
withObjCPtr url $ \raw_url ->
  withObjCPtr errorInfo $ \raw_errorInfo ->
      sendMsg nsAppleScript (mkSelector "initWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSource:@
initWithSource :: (IsNSAppleScript nsAppleScript, IsNSString source) => nsAppleScript -> source -> IO (Id NSAppleScript)
initWithSource nsAppleScript  source =
withObjCPtr source $ \raw_source ->
    sendMsg nsAppleScript (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= ownedObject . castPtr

-- | @- compileAndReturnError:@
compileAndReturnError :: (IsNSAppleScript nsAppleScript, IsNSDictionary errorInfo) => nsAppleScript -> errorInfo -> IO Bool
compileAndReturnError nsAppleScript  errorInfo =
withObjCPtr errorInfo $ \raw_errorInfo ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAppleScript (mkSelector "compileAndReturnError:") retCULong [argPtr (castPtr raw_errorInfo :: Ptr ())]

-- | @- executeAndReturnError:@
executeAndReturnError :: (IsNSAppleScript nsAppleScript, IsNSDictionary errorInfo) => nsAppleScript -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAndReturnError nsAppleScript  errorInfo =
withObjCPtr errorInfo $ \raw_errorInfo ->
    sendMsg nsAppleScript (mkSelector "executeAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_errorInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- executeAppleEvent:error:@
executeAppleEvent_error :: (IsNSAppleScript nsAppleScript, IsNSAppleEventDescriptor event, IsNSDictionary errorInfo) => nsAppleScript -> event -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAppleEvent_error nsAppleScript  event errorInfo =
withObjCPtr event $ \raw_event ->
  withObjCPtr errorInfo $ \raw_errorInfo ->
      sendMsg nsAppleScript (mkSelector "executeAppleEvent:error:") (retPtr retVoid) [argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- source@
source :: IsNSAppleScript nsAppleScript => nsAppleScript -> IO (Id NSString)
source nsAppleScript  =
  sendMsg nsAppleScript (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- compiled@
compiled :: IsNSAppleScript nsAppleScript => nsAppleScript -> IO Bool
compiled nsAppleScript  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAppleScript (mkSelector "compiled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @compileAndReturnError:@
compileAndReturnErrorSelector :: Selector
compileAndReturnErrorSelector = mkSelector "compileAndReturnError:"

-- | @Selector@ for @executeAndReturnError:@
executeAndReturnErrorSelector :: Selector
executeAndReturnErrorSelector = mkSelector "executeAndReturnError:"

-- | @Selector@ for @executeAppleEvent:error:@
executeAppleEvent_errorSelector :: Selector
executeAppleEvent_errorSelector = mkSelector "executeAppleEvent:error:"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @compiled@
compiledSelector :: Selector
compiledSelector = mkSelector "compiled"

