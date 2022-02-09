function ls --wraps='lsd -l' --description 'alias ls=lsd -l'
  lsd -l $argv; 
end
