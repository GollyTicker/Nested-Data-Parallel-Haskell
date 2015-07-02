
headPL :: PA (PA Int) -> PA Int
headPL =
  let headS (DAArr from to segd chunk) =
      let headsChunk = mapS indexS (daarrIndices segd) chunk
      in  DAInt from to n headsChunk
  in  joinD
      . mapD headS
      . splitD

"analog lastPL"
lastPL :: PA (PA Int) -> PA Int
lastPL = joinD . mapD lastS . splitD
