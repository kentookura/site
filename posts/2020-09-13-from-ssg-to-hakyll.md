---
title: a suckless way to write static sites
---

I wrote a little wrapper script for [ssg5](https://www.romanzolotarev.com/ssg.html) for live-updating the browser.
The files for the website are stored in a directory called site:

	$ pwd
	/home/kento/site
	$ ls
	dst/ src/
	$ ls src
	_footer.html _header.html index.md

ssg5 looks to src and generates an .html file for every .md file found in this directory. It also generates sitemap.xml

    $ ssg5 src dst "kento\'s blog" "https://okura.at"
    $ ls dst
    index.html sitemap.xml

I execute ssg5 automatically whenever a file in src changes with entr:

    find src | entr -ps "rm -f dst/.files && ssg5 src dst 'kento\'s blog' 'https://okura.at'"

I then use trap to give me the option to deploy the website upon exiting entr:

    #!/bin/sh

    trap deploy EXIT

    deploy(){
        printf "deploy website now? [y/n] "
        read -r will_deploy
	    case $will_deploy in
	            [Yy]) rsync -avP --delete dst/ root@okura.at:/var/www/main && break;;
		            *) break;; 
			        esac
		}
	find src | entr -ps "rm -f dst/.files && ssg5 src dst 'kento\'s blog' 'https://okura.at'"
				
When I now quit out of entr with Ctrl-C and enter y, rsync will copy all files from dst over to my remote server on which this website is hosted.

While writing, I view the generated local html in Qutebrowser. With it's scripting interface, I can automatically reload the page whenever it is updated, again using entr:

    #!/bin/sh
    echo "$QUTE_URL" | sed 's/^file:\/\///' | entr echo reload >> $QUTE_FIFO
	
Since Qutebrowser is not available on OpenBSD I needed an alternative
way of doing what I described in this [post](maintaining-a-website.html)

I ended up writing this [script](sg), utilizing [surf](https://surf.suckless.org),
entr,
[tabbed](https://tools.suckless.org/tabbed/), [dmenu](https://tools.suckless.org/dmenu) and [ssg5](https://www.romanzolotarev.com/ssg.html).  

Tabbed is a program that allows you to bundle multiple windows into a single tabbed window
using the XEmbed Protocol. 
Running `tabbed` in a terminal will open up a blank window and prints out a unique
identifier. For example:

    $ tabbed
	0x4a00003
	
This is the window-ID of the new tabbed instance and will allow us to embed new windows into it. For example:

    surf -w "0x4a00003" index.html &

Like surf, st can also be embedded in a tabbed instance.

    st -w "0x4200003" -e vim index.md &

Running `sg s` will open two empty `tabbed` instances and saves their xid in a file.
Subsequently running `sg o` opens a selection of markdown files in the directory specified
by the `$SITE_SOURCE_DIR` variable. The selected markdown source file and the corresponding
html file are opened in vim and surf respectiveley and embedded into a tabbed instance.
Upon a save, surf automatically refreshes the file by sending the `kill -HUP` signal.

# managing multiple sites

To start writing a site, create the project root directory and run sg.
This will prompt you for some basic information for the project.

Here is the full script:

::: {.code-include lexer="bash" file="code/sg"}
:::
