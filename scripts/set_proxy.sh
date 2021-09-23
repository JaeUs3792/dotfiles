
echo "===================================="
echo "Proxy Setting enable(y/N)?"
read val

if [ "${val}" == "" ]; then
	echo $val
fi

if [ "${val}" == "y" ];then
	echo "address(ex:192.168.1.168)?"
	read ip
	echo "port(ex:3128)"
	read port
	proxy_addr="http://${ip}:${port}"
	echo proxy address is $proxy_addr

	# zsh
	echo export http_proxy=$proxy_addr >> ~/.zshrc
	echo export HTTP_PROXY=$proxy_addr >> ~/.zshrc
	echo export https_proxy=$proxy_addr >> ~/.zshrc
	echo export HTTPS_PROXY=$proxy_addr >> ~/.zshrc
	echo export ftp_proxy=$proxy_addr >> ~/.zshrc
	echo export FTP_PROXY=$proxy_addr >> ~/.zshrc
	echo export rsync_proxy=$proxy_addr >> ~/.zshrc
	echo export RSYNC_PROXY=$proxy_addr >> ~/.zshrc

	#fish
	echo export http_proxy=$proxy_addr >> ~/.config/fish/config.fish
	echo export HTTP_PROXY=$proxy_addr >> ~/.config/fish/config.fish
	echo export https_proxy=$proxy_addr >> ~/.config/fish/config.fish
	echo export HTTPS_PROXY=$proxy_addr >> ~/.config/fish/config.fish
	echo export ftp_proxy=$proxy_addr >> ~/.config/fish/config.fish
	echo export FTP_PROXY=$proxy_addr >> ~/.config/fish/config.fish
	echo export rsync_proxy=$proxy_addr >> ~/.config/fish/config.fish
	echo export RSYNC_PROXY=$proxy_addr >> ~/.config/fish/config.fish
	#sudo
	echo "Defaults env_keep += \"*_proxy *_PROXY\"" > ./05_proxy
	sudo mv 05_proxy /etc/sudoers.d
	sudo chown root:root /etc/sudoers.d/05_proxy

	#git
	git config --global http.proxy $proxy_addr
	git config --global https.proxy $proxy_addr
fi



