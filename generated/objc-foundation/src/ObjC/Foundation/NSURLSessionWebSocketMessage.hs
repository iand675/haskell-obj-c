{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionWebSocketMessage@.
module ObjC.Foundation.NSURLSessionWebSocketMessage
  ( NSURLSessionWebSocketMessage
  , IsNSURLSessionWebSocketMessage(..)
  , initWithData
  , initWithString
  , init_
  , new
  , type_
  , data_
  , string
  , initWithDataSelector
  , initWithStringSelector
  , initSelector
  , newSelector
  , typeSelector
  , dataSelector
  , stringSelector

  -- * Enum types
  , NSURLSessionWebSocketMessageType(NSURLSessionWebSocketMessageType)
  , pattern NSURLSessionWebSocketMessageTypeData
  , pattern NSURLSessionWebSocketMessageTypeString

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
import ObjC.Foundation.Internal.Enums

-- | @- initWithData:@
initWithData :: (IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage, IsNSData data_) => nsurlSessionWebSocketMessage -> data_ -> IO (Id NSURLSessionWebSocketMessage)
initWithData nsurlSessionWebSocketMessage  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsurlSessionWebSocketMessage (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithString:@
initWithString :: (IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage, IsNSString string) => nsurlSessionWebSocketMessage -> string -> IO (Id NSURLSessionWebSocketMessage)
initWithString nsurlSessionWebSocketMessage  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsurlSessionWebSocketMessage (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage => nsurlSessionWebSocketMessage -> IO (Id NSURLSessionWebSocketMessage)
init_ nsurlSessionWebSocketMessage  =
  sendMsg nsurlSessionWebSocketMessage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSessionWebSocketMessage)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionWebSocketMessage"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- type@
type_ :: IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage => nsurlSessionWebSocketMessage -> IO NSURLSessionWebSocketMessageType
type_ nsurlSessionWebSocketMessage  =
  fmap (coerce :: CLong -> NSURLSessionWebSocketMessageType) $ sendMsg nsurlSessionWebSocketMessage (mkSelector "type") retCLong []

-- | @- data@
data_ :: IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage => nsurlSessionWebSocketMessage -> IO (Id NSData)
data_ nsurlSessionWebSocketMessage  =
  sendMsg nsurlSessionWebSocketMessage (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- string@
string :: IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage => nsurlSessionWebSocketMessage -> IO (Id NSString)
string nsurlSessionWebSocketMessage  =
  sendMsg nsurlSessionWebSocketMessage (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

