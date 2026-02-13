{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterChannelPagingStruct@.
module ObjC.Matter.MTRChannelClusterChannelPagingStruct
  ( MTRChannelClusterChannelPagingStruct
  , IsMTRChannelClusterChannelPagingStruct(..)
  , previousToken
  , setPreviousToken
  , nextToken
  , setNextToken
  , nextTokenSelector
  , previousTokenSelector
  , setNextTokenSelector
  , setPreviousTokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousToken@
previousToken :: IsMTRChannelClusterChannelPagingStruct mtrChannelClusterChannelPagingStruct => mtrChannelClusterChannelPagingStruct -> IO (Id MTRChannelClusterPageTokenStruct)
previousToken mtrChannelClusterChannelPagingStruct =
  sendMessage mtrChannelClusterChannelPagingStruct previousTokenSelector

-- | @- setPreviousToken:@
setPreviousToken :: (IsMTRChannelClusterChannelPagingStruct mtrChannelClusterChannelPagingStruct, IsMTRChannelClusterPageTokenStruct value) => mtrChannelClusterChannelPagingStruct -> value -> IO ()
setPreviousToken mtrChannelClusterChannelPagingStruct value =
  sendMessage mtrChannelClusterChannelPagingStruct setPreviousTokenSelector (toMTRChannelClusterPageTokenStruct value)

-- | @- nextToken@
nextToken :: IsMTRChannelClusterChannelPagingStruct mtrChannelClusterChannelPagingStruct => mtrChannelClusterChannelPagingStruct -> IO (Id MTRChannelClusterPageTokenStruct)
nextToken mtrChannelClusterChannelPagingStruct =
  sendMessage mtrChannelClusterChannelPagingStruct nextTokenSelector

-- | @- setNextToken:@
setNextToken :: (IsMTRChannelClusterChannelPagingStruct mtrChannelClusterChannelPagingStruct, IsMTRChannelClusterPageTokenStruct value) => mtrChannelClusterChannelPagingStruct -> value -> IO ()
setNextToken mtrChannelClusterChannelPagingStruct value =
  sendMessage mtrChannelClusterChannelPagingStruct setNextTokenSelector (toMTRChannelClusterPageTokenStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousToken@
previousTokenSelector :: Selector '[] (Id MTRChannelClusterPageTokenStruct)
previousTokenSelector = mkSelector "previousToken"

-- | @Selector@ for @setPreviousToken:@
setPreviousTokenSelector :: Selector '[Id MTRChannelClusterPageTokenStruct] ()
setPreviousTokenSelector = mkSelector "setPreviousToken:"

-- | @Selector@ for @nextToken@
nextTokenSelector :: Selector '[] (Id MTRChannelClusterPageTokenStruct)
nextTokenSelector = mkSelector "nextToken"

-- | @Selector@ for @setNextToken:@
setNextTokenSelector :: Selector '[Id MTRChannelClusterPageTokenStruct] ()
setNextTokenSelector = mkSelector "setNextToken:"

