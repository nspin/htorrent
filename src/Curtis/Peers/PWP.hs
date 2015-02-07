
data Message = Keepalive
             | Choke
             | Unchoke
             | Intersted
             | Bored
             | Have Integer
             | Bitfield Map Integer Bool
             | Piece Integer Integer B.ByteString
             | Request Integer Integer Integer
             | Cancel Integer Integer Integer
             deriving Show

