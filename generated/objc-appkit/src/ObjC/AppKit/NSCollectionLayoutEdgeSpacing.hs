{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutEdgeSpacing@.
module ObjC.AppKit.NSCollectionLayoutEdgeSpacing
  ( NSCollectionLayoutEdgeSpacing
  , IsNSCollectionLayoutEdgeSpacing(..)
  , spacingForLeading_top_trailing_bottom
  , init_
  , new
  , leading
  , top
  , trailing
  , bottom
  , spacingForLeading_top_trailing_bottomSelector
  , initSelector
  , newSelector
  , leadingSelector
  , topSelector
  , trailingSelector
  , bottomSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ spacingForLeading:top:trailing:bottom:@
spacingForLeading_top_trailing_bottom :: (IsNSCollectionLayoutSpacing leading, IsNSCollectionLayoutSpacing top, IsNSCollectionLayoutSpacing trailing, IsNSCollectionLayoutSpacing bottom) => leading -> top -> trailing -> bottom -> IO (Id NSCollectionLayoutEdgeSpacing)
spacingForLeading_top_trailing_bottom leading top trailing bottom =
  do
    cls' <- getRequiredClass "NSCollectionLayoutEdgeSpacing"
    withObjCPtr leading $ \raw_leading ->
      withObjCPtr top $ \raw_top ->
        withObjCPtr trailing $ \raw_trailing ->
          withObjCPtr bottom $ \raw_bottom ->
            sendClassMsg cls' (mkSelector "spacingForLeading:top:trailing:bottom:") (retPtr retVoid) [argPtr (castPtr raw_leading :: Ptr ()), argPtr (castPtr raw_top :: Ptr ()), argPtr (castPtr raw_trailing :: Ptr ()), argPtr (castPtr raw_bottom :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutEdgeSpacing)
init_ nsCollectionLayoutEdgeSpacing  =
  sendMsg nsCollectionLayoutEdgeSpacing (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutEdgeSpacing)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutEdgeSpacing"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- leading@
leading :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutSpacing)
leading nsCollectionLayoutEdgeSpacing  =
  sendMsg nsCollectionLayoutEdgeSpacing (mkSelector "leading") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- top@
top :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutSpacing)
top nsCollectionLayoutEdgeSpacing  =
  sendMsg nsCollectionLayoutEdgeSpacing (mkSelector "top") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trailing@
trailing :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutSpacing)
trailing nsCollectionLayoutEdgeSpacing  =
  sendMsg nsCollectionLayoutEdgeSpacing (mkSelector "trailing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bottom@
bottom :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutSpacing)
bottom nsCollectionLayoutEdgeSpacing  =
  sendMsg nsCollectionLayoutEdgeSpacing (mkSelector "bottom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @spacingForLeading:top:trailing:bottom:@
spacingForLeading_top_trailing_bottomSelector :: Selector
spacingForLeading_top_trailing_bottomSelector = mkSelector "spacingForLeading:top:trailing:bottom:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @leading@
leadingSelector :: Selector
leadingSelector = mkSelector "leading"

-- | @Selector@ for @top@
topSelector :: Selector
topSelector = mkSelector "top"

-- | @Selector@ for @trailing@
trailingSelector :: Selector
trailingSelector = mkSelector "trailing"

-- | @Selector@ for @bottom@
bottomSelector :: Selector
bottomSelector = mkSelector "bottom"

