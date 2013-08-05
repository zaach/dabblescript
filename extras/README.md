
## vim syntax highlighting

Add dabblescript.vim to your `.vim/syntax` directory and add the following lines to `.vim/filetype.vim`:

    " dabblescript
    au! BufRead,BufNewFile *.dab,*.dabble setfiletype dabblescript
