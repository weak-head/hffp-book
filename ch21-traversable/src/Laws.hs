module Laws where

----------------------------------------------------------------------
-- Traverse:

--- Naturality
-- t . traverse f = traverse (t . f)

--- Identity
-- traverse Identity = Identity

--- Composition
-- traverse (Compose . fmap g . f) =
--   Compose . fmap (traverse g) . traverse f


----------------------------------------------------------------------
-- Sequence:

-- Naturality
-- t . sequenceA = sequenceA . fmap t

-- Identity
-- sequenceA . fmap Identity = Identity

-- Composition
-- sequenceA . fmap Compose =
--   Compose . fmap sequenceA . sequenceA
