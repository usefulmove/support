" ekso drive mapping conversion (osx to windows)
" 26 november 2013


" convert slashes to backslashes
%s/\//\\/g

" drive mapping
%s/\\Volumes\\Berkeley\sBionics/M:/g
%s/\\Volumes\\Accounting/N:/g
%s/\\Volumes\\Corporate\sDocs/O:/g
%s/\\Volumes\\Software/S:/g
%s/\\Volumes\\Users/U:/g


" remove double backslashes
%s/\\\\/\\/g
