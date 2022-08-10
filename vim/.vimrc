" Disable compatibility with vi which can cause unexpected issues.
set nocompatible

" Enable type file detection. Vim will be able to try to detect the type of file in use.
filetype on

" Enable plugins and load plugin for the detected file type.
filetype plugin on

" Load an indent file for the detected file type.
filetype indent on

set cursorline
syntax on
set number
se mouse+=a
set tabstop=4
set shiftwidth=4
set expandtab

"=----MAPPINGS----=================================================
"# copy into clipboard
vnoremap <F5> "+y

""==========VIM-BUNDLE SETTINGS====================================
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
" Plugin 'L9'
" Git plugin not hosted on GitHub
Plugin 'git://git.wincent.com/command-t.git'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
Plugin 'myusuf3/numbers.vim'
Plugin 'tpope/vim-commentary'
"Plugin 'davidhalter/jedi-vim'
Plugin 'tpope/vim-surround'


call vundle#end()            " required
filetype plugin indent on    " required
""==========VIM-BUNDLE SETTINGSEND====================================

"""======---THEME---=================================================
Plugin 'junegunn/seoul256.vim'
" Unified color scheme (default: dark)
colo seoul256

" Light color scheme
colo seoul256-light

" Switch
set background=dark
" set background=light
"""=====---THEME END---================================================

"=======---NERDTree----================================================
Plugin 'scrooloose/nerdtree'
"toggle tree
map <C-t> :NERDTreeToggle<CR>


"=======---NERDTreeTabs----================================================
Plugin 'jistr/vim-nerdtree-tabs'
map <Leader>n <plug>NERDTreeTabsToggle<CR>


