Int product ( [ Int ] list )
{	if ( isEmpty ( list ))
		return 1;
	else
		return head ( list ) * product ( tail ( list ));
}

