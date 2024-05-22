$a = "FiraCode","ComicShannsMono","Mononoki"

foreach($i in $a){
	curl -s https://api.github.com/repos/ryanoasis/nerd-fonts/releases/latest | grep "browser_download_url.*$i.zip" | cut -d : -f 2,3 | tr -d """" | wget -qi -
}

