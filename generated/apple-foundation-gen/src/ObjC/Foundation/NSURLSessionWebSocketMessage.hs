{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dataSelector
  , initSelector
  , initWithDataSelector
  , initWithStringSelector
  , newSelector
  , stringSelector
  , typeSelector

  -- * Enum types
  , NSURLSessionWebSocketMessageType(NSURLSessionWebSocketMessageType)
  , pattern NSURLSessionWebSocketMessageTypeData
  , pattern NSURLSessionWebSocketMessageTypeString

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithData:@
initWithData :: (IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage, IsNSData data_) => nsurlSessionWebSocketMessage -> data_ -> IO (Id NSURLSessionWebSocketMessage)
initWithData nsurlSessionWebSocketMessage data_ =
  sendOwnedMessage nsurlSessionWebSocketMessage initWithDataSelector (toNSData data_)

-- | @- initWithString:@
initWithString :: (IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage, IsNSString string) => nsurlSessionWebSocketMessage -> string -> IO (Id NSURLSessionWebSocketMessage)
initWithString nsurlSessionWebSocketMessage string =
  sendOwnedMessage nsurlSessionWebSocketMessage initWithStringSelector (toNSString string)

-- | @- init@
init_ :: IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage => nsurlSessionWebSocketMessage -> IO (Id NSURLSessionWebSocketMessage)
init_ nsurlSessionWebSocketMessage =
  sendOwnedMessage nsurlSessionWebSocketMessage initSelector

-- | @+ new@
new :: IO (Id NSURLSessionWebSocketMessage)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionWebSocketMessage"
    sendOwnedClassMessage cls' newSelector

-- | @- type@
type_ :: IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage => nsurlSessionWebSocketMessage -> IO NSURLSessionWebSocketMessageType
type_ nsurlSessionWebSocketMessage =
  sendMessage nsurlSessionWebSocketMessage typeSelector

-- | @- data@
data_ :: IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage => nsurlSessionWebSocketMessage -> IO (Id NSData)
data_ nsurlSessionWebSocketMessage =
  sendMessage nsurlSessionWebSocketMessage dataSelector

-- | @- string@
string :: IsNSURLSessionWebSocketMessage nsurlSessionWebSocketMessage => nsurlSessionWebSocketMessage -> IO (Id NSString)
string nsurlSessionWebSocketMessage =
  sendMessage nsurlSessionWebSocketMessage stringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSURLSessionWebSocketMessage)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id NSURLSessionWebSocketMessage)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionWebSocketMessage)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionWebSocketMessage)
newSelector = mkSelector "new"

-- | @Selector@ for @type@
typeSelector :: Selector '[] NSURLSessionWebSocketMessageType
typeSelector = mkSelector "type"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

