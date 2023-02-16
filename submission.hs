-- 26020-lab3-S-Haskell

-- 1.1 REPRESENTING QUADTREES

-- data structure to store the colour of a leaf
data Colour = Black | White deriving (Eq, Show)
-- data structure to store the quadtree
data QuadTrees = Leaf Colour | Node QuadTrees QuadTrees QuadTrees QuadTrees deriving (Eq, Show)

allBlack :: Int -> QuadTrees
allBlack _ = Leaf Black

allWhite :: Int -> QuadTrees
allWhite _ = Leaf White

-- define functions to form a clockwise order
clockwise :: QuadTrees -> QuadTrees -> QuadTrees -> QuadTrees -> QuadTrees
clockwise a b c d = Node a b d c

-- define functions to form an anticlockwise order
anticlockwise :: QuadTrees -> QuadTrees -> QuadTrees -> QuadTrees -> QuadTrees
anticlockwise a b c d = Node a d b c

-- 1.2 A CRUDE 'BLURRING' OPERATION

reverseColour :: Colour -> QuadTrees
reverseColour Black = Leaf White
reverseColour White = Leaf Black

-- data structure to store if there is a node in a direction
data Surrounding = Empty | NTree QuadTrees deriving (Eq, Show)
-- data structure to store the surrounding nodes of a leaf in all directions
data Surroundings = Surroundings {getTop :: Surrounding,
                                  getBottom :: Surrounding,
                                  getLeft :: Surrounding,
                                  getRight :: Surrounding} deriving (Eq, Show)
-- data structure to store the colour of a leaf and its neighbours into a list
data ColourList = LeafList Colour [Colour] | NodeList ColourList ColourList ColourList ColourList deriving (Eq, Show)


-- define functions to get sub-quadtree of a quadtree in a given direction

getNodeA :: Surrounding -> Surrounding
getNodeA Empty = Empty
getNodeA (NTree (Leaf a)) = NTree (Leaf a)
getNodeA (NTree (Node a b c d)) = NTree a

getNodeB :: Surrounding -> Surrounding
getNodeB Empty = Empty
getNodeB (NTree (Leaf a)) = NTree (Leaf a)
getNodeB (NTree (Node a b c d)) = NTree b

getNodeC :: Surrounding -> Surrounding
getNodeC Empty = Empty
getNodeC (NTree (Leaf a)) = NTree (Leaf a)
getNodeC (NTree (Node a b c d)) = NTree c

getNodeD :: Surrounding -> Surrounding
getNodeD Empty = Empty
getNodeD (NTree (Leaf a)) = NTree (Leaf a)
getNodeD (NTree (Node a b c d)) = NTree d


-- define functions to get leaves of a quadtree in a given direction

getTopLeaf :: Surrounding -> [Colour]
getTopLeaf Empty = []
getTopLeaf (NTree (Leaf a)) = [a]
getTopLeaf (NTree (Node a b c d)) = getTopLeaf (NTree a) ++ getTopLeaf (NTree b)

getBottomLeaf :: Surrounding -> [Colour]
getBottomLeaf Empty = []
getBottomLeaf (NTree (Leaf a)) = [a]
getBottomLeaf (NTree (Node a b c d)) = getBottomLeaf (NTree c) ++ getBottomLeaf (NTree d)

getLeftLeaf :: Surrounding -> [Colour]
getLeftLeaf Empty = []
getLeftLeaf (NTree (Leaf a)) = [a]
getLeftLeaf (NTree (Node a b c d)) = getLeftLeaf (NTree a) ++ getLeftLeaf (NTree c)

getRightLeaf :: Surrounding -> [Colour]
getRightLeaf Empty = []
getRightLeaf (NTree (Leaf a)) = [a]
getRightLeaf (NTree (Node a b c d)) = getRightLeaf (NTree b) ++ getRightLeaf (NTree d)

-- update the neighbours of a leaf into a list
updateNeighbours :: ColourList -> Surroundings -> ColourList
updateNeighbours (LeafList a list) neighbours = LeafList a (list ++ getBottomLeaf (getTop neighbours)
                                                                 ++ getTopLeaf (getBottom neighbours)
                                                                 ++ getRightLeaf (getLeft neighbours)
                                                                 ++ getLeftLeaf (getRight neighbours))
-- pass sub-quadtree to form use sub-surroundings
updateNeighbours (NodeList a b c d) neighbours = NodeList
  (updateNeighbours a (Surroundings {getTop = getNodeC (getTop neighbours),
                                     getBottom = Empty,
                                     getLeft = getNodeB (getLeft neighbours),
                                     getRight = Empty}))
  (updateNeighbours b (Surroundings {getTop = getNodeD (getTop neighbours),
                                     getBottom = Empty,
                                     getLeft = Empty,
                                     getRight = getNodeA (getRight neighbours)}))
  (updateNeighbours c (Surroundings {getTop = Empty,
                                     getBottom = getNodeA (getBottom neighbours),
                                     getLeft = getNodeD (getLeft neighbours),
                                     getRight = Empty}))
  (updateNeighbours d (Surroundings {getTop = Empty,
                                     getBottom = getNodeB (getBottom neighbours),
                                     getLeft = Empty,
                                     getRight = getNodeC (getRight neighbours)}))

-- compute the neighbours of each leaf in a quadtree
computeNeighbours :: QuadTrees -> ColourList
computeNeighbours (Leaf a) = LeafList a []
-- pass surrounding nodes to updateNeighbours
computeNeighbours (Node a b c d) = NodeList
  (updateNeighbours (computeNeighbours a) (Surroundings {getTop = Empty, getBottom = NTree c, getLeft = Empty, getRight = NTree b}))
  (updateNeighbours (computeNeighbours b) (Surroundings {getTop = Empty, getBottom = NTree d, getLeft = NTree a, getRight = Empty}))
  (updateNeighbours (computeNeighbours c) (Surroundings {getTop = NTree a, getBottom = Empty, getLeft = Empty, getRight = NTree d}))
  (updateNeighbours (computeNeighbours d) (Surroundings {getTop = NTree b, getBottom = Empty, getLeft = NTree c, getRight = Empty}))

-- count the number of black and white neighbours of a leaf to determine the colour of the leaf
-- (black if more than half neighbours are black, white otherwise. If equal, keep the original colour)
countNeighbours :: Colour -> [Colour] -> Bool
countNeighbours Black [] = False
countNeighbours White [] = False
countNeighbours Black x = if length(filter (==Black) x) < length(filter (==White) x) then True else False
countNeighbours White x = if length(filter (==White) x) < length(filter (==Black) x) then True else False

-- compute the colour change of a leaf
computeChange :: ColourList -> QuadTrees
computeChange (LeafList a list) = if countNeighbours a list == True then (reverseColour a) else Leaf a
computeChange (NodeList a b c d) = Node (computeChange a) (computeChange b) (computeChange c) (computeChange d)

-- update the colour of a leaf
blur :: QuadTrees -> QuadTrees
blur (Leaf a) = Leaf a
blur (Node a b c d) = computeChange (computeNeighbours (Node a b c d))
