/∗
Three ways to implement the factorial function in SPL.
First the recursive version .
∗/
Int facR ( Int n )
{
	if ( n < 2 )
		return 1;
	else
		return n ∗ facR ( n − 1 ) ;
}

