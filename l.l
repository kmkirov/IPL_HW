
%%

"test"   printf("pesho");

%%


int yywrap()
{
    return 1;
}

int main(int argc, const char* argv[])
{
    ++argv, --argc;  /* skip over program name */
    if ( argc > 0 )
            yyin = fopen( argv[0], "r" );
    else
            yyin = stdin;

    /* YY_BUFFER_STATE hello = yy_scan_string("(x)=>{ return x !== 1; }"); */
    /* yypush_buffer_state(hello); */
    yylex();
    /* yy_delete_buffer(hello); */
    return 0;
}