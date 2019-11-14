create_tridiagonal<-function(n=5,d1=1,d2=2){output_matrix<-matrix(ncol=n,nrow=n,0)
for(row in 1:(n-1)){output_matrix[row,row-1]=d2
output_matrix[row,row+1]=d2}
output_matrix[row-1,row]=d2
output_matrix[row+1,row]=d2
diag(output_matrix)=d1
return(output_matrix)
}