{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains a web link that the data detection system matches.
--
-- The DataDetection framework returns a link match in a @DDMatchLink@ object, which contains a <doc://com.apple.documentation/documentation/foundation/url> (Swift) or <doc://com.apple.documentation/documentation/foundation/nsurl> (Objective-C).
--
-- Generated bindings for @DDMatchLink@.
module ObjC.DataDetection.DDMatchLink
  ( DDMatchLink
  , IsDDMatchLink(..)
  , url
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An address for a web resource, such as a webpage or image.
--
-- ObjC selector: @- URL@
url :: IsDDMatchLink ddMatchLink => ddMatchLink -> IO (Id NSURL)
url ddMatchLink =
  sendMessage ddMatchLink urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

