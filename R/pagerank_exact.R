pr = function (H, b, alpha = 0.85) {

  n = dim(H)[1]
  # normalize adjacency matrix by row sum and 
  # replace dangling rows with bookmark vector (S matrix)
  S = H
  # compute row sums
  rs = H %*% rep(1,n)
  for (i in 1:n) {
    if (rs[i] == 0) {
      S[i,] = b
    } else {
      S[i,] = S[i,] / rs[i]   
    }  
  }

  # build teleportation matrix 
  T = rep(1, n) %*% t(b)

  # build Google matrix 
  G = alpha * S + (1-alpha) * T

  # compute eigenpairs and retrieve the leading eigenvector
  eig = eigen(t(G))
  pi = as.real(eig$vectors[,1])
  pi = pi / sum(pi)
  return(pi)
}
