pushd c:\tools

$a = "FiraCode","ComicShannsMono","Mononoki"

foreach($i in $a){
	curl -s https://api.github.com/repos/ryanoasis/nerd-fonts/releases/latest | grep "browser_download_url.*$i.zip" | cut -d : -f 2,3 | tr -d """" | wget -qi -
}


$files = Get-ChildItem .\*.zip
mkdir Fonts
foreach($file in $files){
	unzip -o $file -d .\Fonts
}

cp Fonts\*.ttf c:\windows\fonts
cp Fonts\*.otf c:\windows\fonts

popd 


