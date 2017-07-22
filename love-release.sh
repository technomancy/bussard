#!/usr/bin/env bash

# LÖVE version
LOVE_DEF_VERSION=0.10.2



# Helper functions

# Dependencies check
check_deps () {
    command -v curl  > /dev/null 2>&1 || {
        >&2 echo "curl is not installed. Aborting."
        local EXIT=true
    }
    command -v zip   > /dev/null 2>&1 || {
        >&2 echo "zip is not installed. Aborting."
        local EXIT=true
    }
    command -v unzip > /dev/null 2>&1 || {
        >&2 echo "unzip is not installed. Aborting."
        local EXIT=true
    }
    command -v getopt > /dev/null 2>&1 || {
        local opt=false
    } && {
        unset GETOPT_COMPATIBLE
        local out=$(getopt -T)
        if (( $? != 4 )) && [[ -n $out ]]; then
            local opt=false
        fi
    }
    if [[ $opt == false ]]; then
        >&2 echo "GNU getopt is not installed. Aborting."
        local EXIT=true
    fi
    if ! command -v readlink > /dev/null 2>&1 || ! readlink -m / > /dev/null 2>&1; then
        command -v greadlink > /dev/null 2>&1 || {
            >&2 echo "GNU readlink is not installed. Aborting."
            local EXIT=true
        } && {
            readlink () {
                greadlink "$@"
            }
        }
    fi

    command -v lua   > /dev/null 2>&1 || {
        echo "lua is not installed. Install it to ease your releases."
    } && {
        LUA=true
    }
    if [[ $EXIT == true ]]; then
        exit_module "deps"
    fi
}

# Get user confirmation, simple Yes/No question
## $1: message, usually just a question
## $2: default choice, 0 - yes; 1 - no, default - yes
## return: 0 - yes, 1 - no
get_user_confirmation () {
    if [[ -z $2 || $2 == "0" ]]; then
        read -n 1 -p "$1 [Y/n]: " yn
        local default=0
    else
        read -n 1 -p "$1 [y/N]: " yn
        local default=1
    fi
    case $yn in
        [Yy]* )
            echo; return 0;;
        [Nn]* )
            echo; return 1;;
        "" )
            return $default;;
        * )
            echo; return $default;;
    esac
}


# Generate LÖVE version variables
## $1: LÖVE version string
## return: 0 - string matched, 1 - else
gen_version () {
    if [[ $1 =~ ^([0-9]+)\.([0-9]+)\.([0-9]+)$ ]]; then
        LOVE_VERSION=$1
        LOVE_VERSION_MAJOR=${BASH_REMATCH[1]}
        LOVE_VERSION_MINOR=${BASH_REMATCH[2]}
        LOVE_VERSION_REVISION=${BASH_REMATCH[3]}
        return 0
    fi
    return 1
}


# Compare two LÖVE versions
## $1: First LÖVE version
## $2: comparison operator
##     "ge", "le", "gt" "lt"
##     ">=", "<=", ">", "<"
## $3: Second LÖVE version
## return: 0 - true, 1 - false
compare_version () {
    if [[ $1 =~ ^([0-9]+)\.([0-9]+)\.([0-9]+)$ ]]; then
        local v1_maj=${BASH_REMATCH[1]}
        local v1_min=${BASH_REMATCH[2]}
        local v1_rev=${BASH_REMATCH[3]}
    else
        return 1
    fi
    if [[ $3 =~ ^([0-9]+)\.([0-9]+)\.([0-9]+)$ ]]; then
        local v2_maj=${BASH_REMATCH[1]}
        local v2_min=${BASH_REMATCH[2]}
        local v2_rev=${BASH_REMATCH[3]}
    else
        return 1
    fi

    case $2 in
        ge|\>= )
            if (( $v1_maj >= $v2_maj && $v1_min >= $v2_min && $v1_rev >= $v2_rev )); then
                return 0
            else
                return 1
            fi
            ;;
        le|\<= )
            if (( $v1_maj <= $v2_maj && $v1_min <= $v2_min && $v1_rev <= $v2_rev )); then
                return 0
            else
                return 1
            fi
            ;;
        gt|\> )
            if (( $v1_maj > $v2_maj || ( $v1_max == $v2_max && $v1_min > $v2_min ) ||
                ( $v1_max == $v2_max && $v1_min == $v2_min && $v1_rev > $v2_rev ) )); then
                return 0
            else
                return 1
            fi
            ;;
        lt|\< )
            if (( $v1_maj < $v2_maj || ( $v1_max == $v2_max && $v1_min < $v2_min ) ||
                ( $v1_max == $v2_max && $v1_min == $v2_min && $v1_rev < $v2_rev ) )); then
                return 0
            else
                return 1
            fi
            ;;
    esac
}


# Read configuration
## $1: system name
read_config () {
    if [[ $LUA == true ]] && [[ -f "conf.lua" ]]; then
        local var=$(lua - <<EOF
f = loadfile("conf.lua")
t, love = {window = {}, modules = {}, screen = {}}, {}
f()
love.conf(t)

-- "love", "windows", "osx", "debian" or "android"
os = "$1"

fields = {
    "identity", "version", "game_version", "icon", "exclude",
    "title", "author", "email", "url", "description", }

for _, f in ipairs(fields) do
    t[f] = t[f] or ""
end

t.os = t.os or {}
for _, v in ipairs(t.os) do
    t.os[v] = {}
end

if os == "default" then
    t.os.default = {}
end

if t.os[os] then
    print(os:upper()..'=true')
    for _, f in ipairs(fields) do
        t.os[os][f] = t.os[os][f] or t[f]
        if type(t.os[os][f]) == "table" then
            str = f:upper()..'=('
            for _, v in ipairs(t.os[os][f]) do
                str = str..' "'..v..'"'
            end
            str = str..' )'
            print(str)
        else
            print(f:upper()..'="'..t.os[os][f]..'"')
        end
    end
else
    print(os:upper()..'=false')
end

if t.os.windows and os == "windows" then
    t.os.windows.x32 = t.os.windows.x32 and true or false
    t.os.windows.x64 = t.os.windows.x64 and true or false
    t.os.windows.installer = t.os.windows.installer and true or false
    t.os.windows.appid = t.os.windows.appid or ""
    print("X32="..tostring(t.os.windows.x32))
    print("X64="..tostring(t.os.windows.x64))
    print("INSTALLER="..tostring(t.os.windows.installer))
    print("APPID="..t.os.windows.appid)
end
EOF
)
        eval "$var"
    fi
}


# Read script options
## $1: options prefix
read_options () {
    local pre="$1"
    eval set -- "$ARGS"
    while true; do
        case "$1" in
            -a|--${pre}author )       AUTHOR="$2"; shift 2 ;;
            --${pre}clean )           rm -rf "$CACHE_DIR"; shift ;;
            -d|--${pre}description )  DESCRIPTION="$2"; shift 2 ;;
            -e|--${pre}email )        EMAIL="$2"; shift 2 ;;
            -h|--${pre}help )         short_help; exit 0 ;;
            -i|--${pre}icon )
                ICON="$2"
                if [[ -d $ICON ]]; then
                    local icon="$(readlink -m "$ICON")"
                    local wd="$(readlink -m "$PWD")"
                    EXCLUDE+=( "${icon//$wd\/}/*" )
                elif [[ -f $ICON ]]; then
                    EXCLUDE+=( "$ICON" )
                fi
                shift 2 ;;
            -l|--${pre}love )         if ! gen_version "$2"; then exit_module "version"; fi; shift 2 ;;
            -p|--${pre}pkg )          IDENTITY="$2"; shift 2 ;;
            -r|--${pre}release )      RELEASE_DIR="$2"; shift 2 ;;
            -t|--${pre}title )        TITLE="$2"; shift 2 ;;
            -u|--${pre}url )          URL="$2"; shift 2 ;;
            -v|--${pre}version )      GAME_VERSION="$2"; shift 2 ;;
            -x|--${pre}exclude )      EXCLUDE+=( "$2" ); shift 2 ;;
            -- ) shift; break ;;
            * ) shift ;;
        esac
    done
}


# Test if default module should be executed
default_module () {
    if [[ $? -ne 2 ]]; then
        DEFAULT_MODULE=false
    fi
}

# Print short help
short_help () {
    cat <<EndOfSHelp
Usage: love-release.sh [options...] [files...]
Options:
 -h           Print short help
 -t <title>   Set the project's title
 -r <release> Set the release directory
 -v <version> Set the LÖVE version
Modules:
 -L    LÖVE
\ -A    Android\
\ -D    Debian\
\ -M    Mac OS X\
\ -W    Windows
EndOfSHelp
}

dump_var () {
    echo "LOVE_VERSION=$LOVE_VERSION"
    echo "LOVE_DEF_VERSION=$LOVE_DEF_VERSION"
    echo "LOVE_WEB_VERSION=$LOVE_WEB_VERSION"
    echo
    echo "RELEASE_DIR=$RELEASE_DIR"
    echo "CACHE_DIR=$CACHE_DIR"
    echo
    echo "IDENTITY=$IDENTITY"
    echo "GAME_VERSION=$GAME_VERSION"
    echo "ICON=$ICON"
    echo
    echo "TITLE=$TITLE"
    echo "AUTHOR=$AUTHOR"
    echo "EMAIL=$EMAIL"
    echo "URL=$URL"
    echo "DESCRIPTION=$DESCRIPTION"
    echo
    echo "${FILES[@]}"
}


# Modules functions

# Init module
## $1: Pretty module name
## $2: Configuration module name
## $3: Module option
## return: 0 - if module should be executed, else exit 2
init_module () {
    (
        opt="$3"
        if (( ${#opt} == 1 )); then opt="-$opt"
        elif (( ${#opt} >= 2 )); then opt="--$opt"; fi
        eval set -- "$ARGS"
        while true; do
            case "$1" in
                $opt ) exit 0 ;;
                -- )   exit 1 ;;
                * )    shift ;;
            esac
        done
    )
    local opt=$?
    local module="$2"
    read_config "$module"
    module=${module^^}
    if (( $opt == 0 )); then
        if [[ ${!module} == false ]]; then
            read_config "default"
        fi
    else
        if [[ ${!module} == false ]]; then
            exit_module "execute"
        fi
    fi
    gen_version $VERSION
    unset VERSION
    MODULE="$1"
    CACHE_DIR="$CACHE_DIR/$2"
    read_options "$3"
    LOVE_FILE="${TITLE}.love"
    mkdir -p "$RELEASE_DIR" "$CACHE_DIR"
    echo "Generating $TITLE with LÖVE $LOVE_VERSION for ${MODULE}..."
    return 0
}

# Create the LÖVE file
## $1: Compression level 0-9
create_love_file () {
    local dotfiles=()
    for file in .*; do
        if [[ $file == '.' || $file == '..' ]]; then continue; fi
        if [[ -d $file ]]; then file="$file/*"; fi
        dotfiles+=( "$file" )
    done
    local release_dir="$(readlink -m "$RELEASE_DIR")"
    local wd="$(readlink -m "$PWD")"
    zip -FS -$1 -r "$RELEASE_DIR/$LOVE_FILE" \
        -x "$0" "${release_dir//$wd\/}/*" "${dotfiles[@]}" "${EXCLUDE[@]}" @ \
        "${FILES[@]}"
}

# Exit module
## $1: optional error identifier
## $2: optional error message
exit_module () {
    if [[ -z $1 ]]; then
        echo "Done !"
        exit 0
    fi
    if [[ -n $2 ]]; then
        >&2 echo -e "$2"
    fi
    case $1 in
        execute )
            exit 2 ;;
        binary )
            >&2 echo "LÖVE $LOVE_VERSION could not be found or downloaded."
            exit 3 ;;
        options )
            exit 4 ;;
        version )
            >&2 echo "LÖVE version string is invalid."
            exit 5 ;;
        deps )
            exit 6 ;;
        undef|* )
            exit 1 ;;
    esac
}



# Main

check_deps

# Get latest LÖVE version number
gen_version $LOVE_DEF_VERSION
LOVE_WEB_VERSION=$(curl -s https://love2d.org/releases.xml | grep -m 2 "<title>" | tail -n 1 | grep -Eo "[0-9]+.[0-9]+.[0-9]+")
gen_version $LOVE_WEB_VERSION

INSTALLED=false
EMBEDDED=true

DEFAULT_MODULE=true

TITLE="$(basename $(pwd))"
PROJECT_DIR="$PWD"
RELEASE_DIR=releases
CACHE_DIR=~/.cache/love-release
FILES=()
EXCLUDE=()

OPTIONS="W::MDALa:d:e:hi:l:p:r:t:u:v:x:"
LONG_OPTIONS="Wauthor:,Wclean,Wdescription:,Wemail:,Wexclude:,Whelp,Wicon:,Wlove:,Wpkg:,Wrelease:,Wtitle:,Wurl:,Wversion:,Wappid:,Winstaller,Mauthor:,Mclean,Mdescription:,Memail:,Mexclude:,Mhelp,Micon:,Mlove:,Mpkg:,Mrelease:,Mtitle:,Murl:,Mversion:,Dauthor:,Dclean,Ddescription:,Demail:,Dexclude:,Dhelp,Dicon:,Dlove:,Dpkg:,Drelease:,Dtitle:,Durl:,Dversion:,Aauthor:,Aclean,Adescription:,Aemail:,Aexclude:,Ahelp,Aicon:,Alove:,Apkg:,Arelease:,Atitle:,Aurl:,Aversion:,Aactivity:,Aupdate,author:,clean,description:,email:,exclude:,help,icon:,love:,pkg:,release:,title:,url:,version:"
ARGS=$(getopt -o "$OPTIONS" -l "$LONG_OPTIONS" -n 'love-release' -- "$@")
if (( $? != 0 )); then short_help; exit_module "options"; fi
eval set -- "$ARGS"
read_options
while [[ $1 != '--' ]]; do shift; done; shift
for arg do
    FILES+=( "$arg" )
done
if (( ${#FILES} == 0 )); then FILES+=( "." ); fi
eval set -- "$ARGS"

if [[ $INSTALLED == false && $EMBEDDED == false ]]; then
    exit_module "undef" "love-release has not been installed, and is not embedded into one script."
fi

if [[ ! -f "main.lua" ]]; then
    >&2 echo "No main.lua file was found."
    exit_module 1
fi

if [[ $EMBEDDED == true ]]; then
    : # include_scripts_here
(source <(cat <<\EndOfModule
# Android debug package
init_module "Android" "android" "A"
OPTIONS="A"
LONG_OPTIONS="activity:,update"


IDENTITY=$(echo $TITLE | sed -e 's/[^-a-zA-Z0-9_]/-/g' | tr '[:upper:]' '[:lower:]')
ACTIVITY=$(echo $TITLE | sed -e 's/[^a-zA-Z0-9_]/_/g')


# Options
while true; do
    case "$1" in
        --Aactivity ) ACTIVITY="$2"; shift 2 ;;
        --Aupdate )   UPDATE_ANDROID=true; shift ;;
        -- ) break ;;
        * ) shift ;;
    esac
done


# Android
missing_info=false
missing_deps=false
error_msg="Could not build Android package."
if ! command -v git > /dev/null 2>&1; then
    missing_deps=true
    error_msg="$error_msg\ngit was not found."
fi
if ! command -v ndk-build > /dev/null 2>&1; then
    missing_deps=true
    error_msg="$error_msg\nndk-build was not found."
fi
if ! command -v ant > /dev/null 2>&1; then
    missing_deps=true
    error_msg="$error_msg\nant was not found."
fi
if [[ $missing_deps == true  ]]; then
    exit_module "deps" "$error_msg"
fi

if [[ -z $GAME_VERSION ]]; then
    missing_info=true
    error_msg="$error_msg\nMissing project's version. Use -v or --Aversion."
fi
if [[ -z $AUTHOR ]]; then
    missing_info=true
    error_msg="$error_msg\nMissing maintainer's name. Use -a or --Aauthor."
fi
if [[ $missing_info == true  ]]; then
    exit_module "options" "$error_msg"
fi


create_love_file 0


LOVE_ANDROID_DIR="$CACHE_DIR/love-android-sdl2"
if [[ -d $LOVE_ANDROID_DIR ]]; then
    cd "$LOVE_ANDROID_DIR"
    git checkout -- .
    rm -rf src/com bin gen
    if [[ $UPDATE_ANDROID = true ]]; then
        LOCAL=$(git rev-parse @)
        REMOTE=$(git rev-parse @{u})
        BASE=$(git merge-base @ @{u})
        if [[ $LOCAL == $REMOTE ]]; then
            echo "love-android-sdl2 is already up-to-date."
        elif [[ $LOCAL == $BASE ]]; then
            git pull
            ndk-build --jobs $(( $(nproc) + 1))
        fi
    fi
else
    cd "$CACHE_DIR"
    git clone https://bitbucket.org/MartinFelis/love-android-sdl2.git
    cd "$LOVE_ANDROID_DIR"
    ndk-build --jobs $(( $(nproc) + 1))
fi

ANDROID_VERSION=$(grep -Eo -m 1 "[0-9]+.[0-9]+.[0-9]+[a-z]*" "$LOVE_ANDROID_DIR"/AndroidManifest.xml)
ANDROID_LOVE_VERSION=$(echo "$ANDROID_VERSION" | grep -Eo "[0-9]+.[0-9]+.[0-9]+")

if [[ "$LOVE_VERSION" != "$ANDROID_LOVE_VERSION" ]]; then
    exit_module 1 "Love version ($LOVE_VERSION) differs from love-android-sdl2 version ($ANDROID_LOVE_VERSION). Could not create package."
fi

mkdir -p assets
cd "$PROJECT_DIR"
cd "$RELEASE_DIR"
cp "$LOVE_FILE" "$LOVE_ANDROID_DIR/assets/game.love"
cd "$LOVE_ANDROID_DIR"

sed -i.bak -e "s/org.love2d.android/com.${AUTHOR}.${IDENTITY}/" \
    -e "s/$ANDROID_VERSION/${ANDROID_VERSION}-${IDENTITY}-v${GAME_VERSION}/" \
    -e "0,/LÖVE for Android/s//$TITLE $GAME_VERSION/" \
    -e "s/LÖVE for Android/$TITLE/" \
    -e "s/GameActivity/$ACTIVITY/" \
    AndroidManifest.xml

mkdir -p "src/com/$AUTHOR/$IDENTITY"
cat > "src/com/$AUTHOR/$IDENTITY/${ACTIVITY}.java" <<EOF
package com.${AUTHOR}.${IDENTITY};
import org.love2d.android.GameActivity;

public class $ACTIVITY extends GameActivity {}
EOF

if [[ -d "$ICON" ]]; then
    cd "$PROJECT_DIR"
    cd "$ICON"

    for icon in *; do
        RES=$(echo "$icon" | grep -Eo "[0-9]+x[0-9]+")
        EXT=$(echo "$icon" | sed -e 's/.*\.//g')
        if [[ $RES == "42x42" ]]; then
            cp "$icon" "$LOVE_ANDROID_DIR/res/drawable-mdpi/ic_launcher.png"
        elif [[ $RES == "72x72" ]]; then
            cp "$icon" "$LOVE_ANDROID_DIR/res/drawable-hdpi/ic_launcher.png"
        elif [[ $RES == "96x96" ]]; then
            cp "$icon" "$LOVE_ANDROID_DIR/res/drawable-xhdpi/ic_launcher.png"
        elif [[ $RES == "144x144" ]]; then
            cp "$icon" "$LOVE_ANDROID_DIR/res/drawable-xxhdpi/ic_launcher.png"
        elif [[ "$RES" == "732x412" ]]; then
            cp "$icon" "$LOVE_ANDROID_DIR/res/drawable-xhdpi/ouya_icon.png"
        fi
    done
    if [[ -f "drawable-mdpi/ic_launcher.png" ]]; then
        cp "drawable-mdpi/ic_launcher.png" "$LOVE_ANDROID_DIR/res/drawable-mdpi/ic_launcher.png"
    fi
    if [[ -f "drawable-hdpi/ic_launcher.png" ]]; then
        cp "drawable-hdpi/ic_launcher.png" "$LOVE_ANDROID_DIR/res/drawable-hdpi/ic_launcher.png"
    fi
    if [[ -f "drawable-xhdpi/ic_launcher.png" ]]; then
        cp "drawable-xhdpi/ic_launcher.png" "$LOVE_ANDROID_DIR/res/drawable-xhdpi/ic_launcher.png"
    fi
    if [[ -f "drawable-xxhdpi/ic_launcher.png" ]]; then
        cp "drawable-xxhdpi/ic_launcher.png" "$LOVE_ANDROID_DIR/res/drawable-xxhdpi/ic_launcher.png"
    fi
    if [[ -f "drawable-xhdpi/ouya_icon.png" ]]; then
        cp "drawable-xhdpi/ouya_icon.png" "$LOVE_ANDROID_DIR/res/drawable-xhdpi/ouya_icon.png"
    fi

    cd "$LOVE_ANDROID_DIR"
fi


ant debug
cd "$PROJECT_DIR"
cp "$LOVE_ANDROID_DIR/bin/love_android_sdl2-debug.apk" "$RELEASE_DIR"
git checkout -- .
rm -rf src/com bin gen


exit_module
EndOfModule
))
default_module


(source <(cat <<\EndOfModule
# Debian package
init_module "Debian" "debian" "D"
OPTIONS="D"
LONG_OPTIONS=""


IDENTITY=$(echo $TITLE | sed -e 's/[^-a-zA-Z0-9_]/-/g' | tr '[:upper:]' '[:lower:]')

# Debian
missing_info=false
error_msg="Could not build Debian package."
if [[ -z $GAME_VERSION ]]; then
    missing_info=true
    error_msg="$error_msg\nMissing project's version. Use -v or --Dversion."
fi
if [[ -z $URL ]]; then
    missing_info=true
    error_msg="$error_msg\nMissing project's homepage. Use -u or -Durl."
fi
if [[ -z $DESCRIPTION ]]; then
    missing_info=true
    error_msg="$error_msg\nMissing project's description. Use -d or --Ddescription."
fi
if [[ -z $AUTHOR ]]; then
    missing_info=true
    error_msg="$error_msg\nMissing maintainer's name. Use -a or --Dauthor."
fi
if [[ -z $EMAIL ]]; then
    missing_info=true
    error_msg="$error_msg\nMissing maintainer's email. Use -e or --Demail."
fi
if [[ $missing_info == true  ]]; then
    exit_module "options" "$error_msg"
fi


create_love_file 9
cd "$RELEASE_DIR"


TEMP="$(mktemp -d)"
umask 0022

mkdir -p "$TEMP/DEBIAN"
cat > "$TEMP/DEBIAN/control" <<EOF
Package: $IDENTITY
Version: $GAME_VERSION
Architecture: all
Maintainer: $AUTHOR <$EMAIL>
Installed-Size: $(( $(stat -c %s "$LOVE_FILE") / 1024 ))
Depends: love (>= $LOVE_VERSION)
Priority: extra
Homepage: $URL
Description: $DESCRIPTION
EOF

mkdir -p "$TEMP/usr/share/applications"
cat > "$TEMP/usr/share/applications/${IDENTITY}.desktop" <<EOF
[Desktop Entry]
Name=$TITLE
Comment=$DESCRIPTION
Exec=$IDENTITY
Type=Application
Categories=Game;
EOF

mkdir -p "$TEMP/usr/bin"
cat <(echo -ne '#!/usr/bin/env love\n') "$LOVE_FILE" > "$TEMP/usr/bin/$IDENTITY"
chmod +x "$TEMP/usr/bin/$IDENTITY"

if [[ -d $ICON ]]; then
    ICON_LOC=$TEMP/usr/share/icons/hicolor
    mkdir -p $ICON_LOC
    echo "Icon=$IDENTITY" >> "$TEMP/usr/share/applications/${IDENTITY}.desktop"

    cd "$ICON"
    for file in *; do
        RES=$(echo "$file" | grep -Eo "[0-9]+x[0-9]+")
        EXT=$(echo "$file" | sed -e 's/.*\.//g')
        if [[ $EXT == "svg" ]]; then
            mkdir -p "$ICON_LOC/scalable/apps"
            cp "$file" "$ICON_LOC/scalable/apps/${IDENTITY}.svg"
            chmod 0644 "$ICON_LOC/scalable/apps/${IDENTITY}.svg"
        elif [[ -n $RES ]]; then
            mkdir -p "$ICON_LOC/$RES/apps"
            cp "$file" "$ICON_LOC/$RES/apps/${IDENTITY}.$EXT"
            chmod 0644 "$ICON_LOC/$RES/apps/${IDENTITY}.$EXT"
        fi
    done
else
    echo "Icon=love" >> "$TEMP/usr/share/applications/${IDENTITY}.desktop"
fi

cd "$TEMP"
find "usr" -type f -exec md5sum {} \; | sed -E "s/^([0-9a-f]{32}  )/\1\//g" > "$TEMP/DEBIAN/md5sums"
cd "$PROJECT_DIR"

fakeroot dpkg-deb -b "$TEMP" "$RELEASE_DIR/$IDENTITY-${GAME_VERSION}_all.deb"
rm -rf "$TEMP"


exit_module
EndOfModule
))
default_module


(source <(cat <<\EndOfModule
# Mac OS X
init_module "Mac OS X" "osx" "M"
OPTIONS="M"
LONG_OPTIONS=""


IDENTITY=$(echo $TITLE | sed -e 's/[^-a-zA-Z0-9_]/-/g' | tr '[:upper:]' '[:lower:]')

if [[ -z $AUTHOR ]]; then
    exit_module "options" "Missing maintainer's name. Use -a or --Mauthor."
fi
if [[ -z $GAME_VERSION ]]; then
    GAME_VERSION="$LOVE_VERSION"
fi

if [[ -n $ICON ]]; then
    if [[ -d $ICON ]]; then
        for file in $ICON/*.icns; do
            if [[ -f $file ]]; then
                ICON="$file"
                break
            else
                found=false
            fi
        done
    fi
    if [[ $found == false || ! -f $ICON ]]; then
        >&2 echo "OS X icon was not found in ${ICON}."
        icon=Love.icns
        ICON=
    else
        icon="${IDENTITY}.icns"
    fi
fi


create_love_file 9
cd "$RELEASE_DIR"


## MacOS 64-bits ##
if compare_version "$LOVE_VERSION" '>=' '0.9.0'; then
    if [[ ! -f "$CACHE_DIR/love-$LOVE_VERSION-macosx-x64.zip" ]]; then
        curl -L -C - -o $CACHE_DIR/love-$LOVE_VERSION-macosx-x64.zip https://bitbucket.org/rude/love/downloads/love-$LOVE_VERSION-macosx-x64.zip
    fi
    unzip -qq "$CACHE_DIR/love-$LOVE_VERSION-macosx-x64.zip"

    rm -rf "$TITLE-macosx-x64.zip" 2> /dev/null
    mv love.app "${TITLE}.app"
    cp "$LOVE_FILE" "${TITLE}.app/Contents/Resources"
    if [[ -n $ICON ]]; then
        cd "$PROJECT_DIR"
        cp "$ICON" "$RELEASE_DIR/$icon"
        cd "$RELEASE_DIR"
        mv "$icon" "${TITLE}.app/Contents/Resources"
    fi

    sed -i.bak -e '/<key>UTExportedTypeDeclarations<\/key>/,/^\t<\/array>/d' \
        -e "s/>org.love2d.love</>org.${AUTHOR}.$IDENTITY</" \
        -e "s/$LOVE_VERSION/$GAME_VERSION/" \
        -e "s/Love.icns/$icon/" \
        -e "s/>LÖVE</>$TITLE</" \
        "${TITLE}.app/Contents/Info.plist"
    rm "${TITLE}.app/Contents/Info.plist.bak"

    zip -9 -qyr "${TITLE}-macosx-x64.zip" "${TITLE}.app"
    rm -rf love-$LOVE_VERSION-macosx-x64.zip "${TITLE}.app" __MACOSX

    ## MacOS 32-bits ##
else
    if [[ ! -f "$CACHE_DIR/love-$LOVE_VERSION-macosx-ub.zip" ]]; then
        curl -L -C - -o $CACHE_DIR/love-$LOVE_VERSION-macosx-ub.zip https://bitbucket.org/rude/love/downloads/love-$LOVE_VERSION-macosx-ub.zip
    fi
    unzip -qq "$CACHE_DIR/love-$LOVE_VERSION-macosx-ub.zip"

    rm -rf "$TITLE-macosx-ub.zip" 2> /dev/null
    mv love.app "${TITLE}.app"
    cp "$LOVE_FILE" "${TITLE}.app/Contents/Resources"
    if [[ -n $ICON ]]; then
        cd "$PROJECT_DIR"
        cp "$ICON" "$RELEASE_DIR/$icon"
        cd "$RELEASE_DIR"
        mv "$icon" "${TITLE}.app/Contents/Resources"
    fi

    sed -i.bak -e '/<key>UTExportedTypeDeclarations<\/key>/,/^\t<\/array>/d' \
        -e "s/>org.love2d.love</>org.${AUTHOR}.$IDENTITY</" \
        -e "s/$LOVE_VERSION/$GAME_VERSION/" \
        -e "s/Love.icns/$icon/" \
        -e "s/>LÖVE</>$TITLE</" \
        "${TITLE}.app/Contents/Info.plist"
    rm "${TITLE}.app/Contents/Info.plist.bak"

    zip -9 -qyr "${TITLE}-macosx-ub.zip" "${TITLE}.app"
    rm -rf love-$LOVE_VERSION-macosx-ub.zip "${TITLE}.app" __MACOSX
fi


exit_module
EndOfModule
))
default_module


(source <(cat <<\EndOfModule
# Windows
init_module "Windows" "windows" "W"
OPTIONS="W::"
LONG_OPTIONS="appid:,installer"

if [[ -z $IDENTITY ]]; then
    IDENTITY=$(echo $IDENTITY | sed -e 's/[^-a-zA-Z0-9_]/-/g' | tr '[:upper:]' '[:lower:]')
fi

while true; do
    case "$1" in
        --Wappid )     APPID="$2"; shift 2 ;;
        --Winstaller ) INSTALLER=true; shift ;;
        -W )           if [[ -z "$2" ]]; then X32=true; X64=true;
                       elif (( "$2" == 32 )); then X32=true;
                       elif (( "$2" == 64 )); then X64=true;
                       fi; shift ;;
        -- ) break ;;
        * ) shift ;;
    esac
done


FOUND_WINE=true
command -v wine >/dev/null 2>&1 || { FOUND_WINE=false; } && { WINEPREFIX="$CACHE_DIR/wine"; }


if [[ -n $ICON ]]; then
    if [[ $FOUND_WINE == true ]]; then
        if [[ -d $ICON ]]; then
            for file in $ICON/*.ico; do
                if [[ -f $file ]]; then
                    ICON="$file"
                    break
                else
                    found=false
                fi
            done
        fi
        if [[ $found == false || ! -f $ICON ]]; then
            >&2 echo "Windows icon was not found in ${ICON}."
            ICON=
        else
            RESHACKER="$WINEPREFIX/drive_c/Program Files (x86)/Resource Hacker/ResourceHacker.exe"
            if [[ ! -f $RESHACKER ]]; then
                curl -L -C - -o "$WINEPREFIX/drive_c/reshacker_setup.exe" "http://www.angusj.com/resourcehacker/reshacker_setup.exe"
                WINEPREFIX="$WINEPREFIX" wine "$WINEPREFIX/drive_c/reshacker_setup.exe" 2>&1 /dev/null
            fi
        fi
    else
        >&2 echo "Can not set Windows icon without Wine."
    fi
fi


if [[ $INSTALLER == true ]]; then
    missing_opt=false
    error_msg="Could not build Windows installer."
    if [[ $FOUND_WINE == false ]]; then
        >&2 echo "Can not build Windows installer without Wine."
        exit_module "deps"
    fi
    if [[ -z $AUTHOR ]]; then
        missing_opt=true
        error_msg="$error_msg\nMissing project author. Use -a or --Wauthor."
    fi
    if [[ -z $URL ]]; then
        missing_opt=true
        error_msg="$error_msg\nMissing project url. Use -u or --Wurl."
    fi
    if [[ -z $GAME_VERSION ]]; then
        missing_opt=true
        error_msg="$error_msg\nMissing project version. Use -v or --Wversion."
    fi
    if [[ -z $APPID ]]; then
        missing_opt=true
        error_msg="$error_msg\nMissing application GUID. Use --Wappid."
    fi
    if [[ $missing_opt == true ]]; then
        exit_module "options" "$error_msg"
    fi

    INNOSETUP="$WINEPREFIX/drive_c/Program Files (x86)/Inno Setup 5/ISCC.exe"
    if [[ ! -f $INNOSETUP ]]; then
        curl -L -C - -o "$WINEPREFIX/drive_c/is-unicode.exe" "http://www.jrsoftware.org/download.php/is-unicode.exe"
        WINEPREFIX="$WINEPREFIX" wine "$WINEPREFIX/drive_c/is-unicode.exe" 2>&1 /dev/null
    fi

# Inno Setup
# $1: Path to game exe directory
# $2: true if 64 bits release
create_installer () {
    ln -s "$1" "$WINEPREFIX/drive_c/game"
    if [[ -n $ICON ]]; then
        cd "$PROJECT_DIR"
        ln -s "$ICON" "$WINEPREFIX/drive_c/game.ico"
        cd "$RELEASE_DIR"
    else
        ln -s "$1/game.ico" "$WINEPREFIX/drive_c/game.ico"
    fi

    cat > "$WINEPREFIX/drive_c/innosetup.iss" <<EOF
#define MyAppName "$TITLE"
#define MyAppVersion "$GAME_VERSION"
#define MyAppPublisher "$AUTHOR"
#define MyAppURL "$URL"
#define MyAppExeName "${TITLE}.exe"

[Setup]
;ArchitecturesInstallIn64BitMode=x64 ia64
AppId={{$APPID}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputBaseFilename=${IDENTITY}-setup
SetupIconFile=C:\\game.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

[Files]
EOF
    if [[ $2 == true ]]; then
        sed -i 's/;ArchitecturesInstallIn64BitMode/ArchitecturesInstallIn64BitMode/' "$WINEPREFIX/drive_c/innosetup.iss"
    fi

    for file in $1; do
        echo "Source: \"C:\\game\\$file\"; DestDir: \"{app}\"; Flags: ignoreversion" \
            >> "$WINEPREFIX"/drive_c/innosetup.iss
    done

    WINEPREFIX="$WINEPREFIX" wine "$INNOSETUP" /Q 'c:\innosetup.iss'
    mv "$WINEPREFIX/drive_c/Output/$IDENTITY-setup.exe" .
    rm -rf "$WINEPREFIX/drive_c/{game,game.ico,innosetup.iss,Output}"
}

fi

${X32:=false}
${X64:=false}
if [[ $X32 == false && $X64 == false ]]; then
    X32=true
    X64=true
fi


create_love_file 9
cd "$RELEASE_DIR"


# Windows 32-bits
if [[ $X32 == true ]]; then

    if [[ ! -f "$CACHE_DIR/love-$LOVE_VERSION-win32.zip" ]]; then
        if compare_version "$LOVE_VERSION" '>=' '0.9.0'; then
            curl -L -C - -o "$CACHE_DIR/love-$LOVE_VERSION-win32.zip" "https://bitbucket.org/rude/love/downloads/love-$LOVE_VERSION-win32.zip"
        else
            curl -L -C - -o "$CACHE_DIR/love-$LOVE_VERSION-win32.zip" "https://bitbucket.org/rude/love/downloads/love-$LOVE_VERSION-win-x86.zip"
        fi
    fi

    unzip -qq "$CACHE_DIR/love-$LOVE_VERSION-win32.zip"

    if [[ -n $ICON ]]; then
        WINEPREFIX="$WINEPREFIX" wine "$RESHACKER" \
            -addoverwrite "love-$LOVE_VERSION-win32/love.exe,love-$LOVE_VERSION-win32/love.exe,$ICON,ICONGROUP,MAINICON,0" 2>&1 /dev/null
    fi

    cat love-$LOVE_VERSION-win32/love.exe "$LOVE_FILE" > "love-$LOVE_VERSION-win32/${TITLE}.exe"
    rm love-$LOVE_VERSION-win32/love.exe
    mv love-$LOVE_VERSION-win32 "$TITLE"-win32
    if [[ $INSTALLER == true ]]; then
        rm -rf "$IDENTITY-setup-win32.exe" 2> /dev/null
        create_installer "$TITLE-win32"
        mv "$IDENTITY-setup.exe" "$IDENTITY-setup-win32.exe"
    else
        zip -FS -9 -qr "$TITLE-win32.zip" "$TITLE-win32"
    fi
    rm -rf "$TITLE-win32"
fi

## Windows 64-bits ##
if [[ $X64 == true ]] && compare_version "$LOVE_VERSION" '>=' '0.8.0'; then

    if [[ ! -f "$CACHE_DIR/love-$LOVE_VERSION-win64.zip" ]]; then
        if compare_version "$LOVE_VERSION" '>=' '0.9.0'; then
            curl -L -C - -o "$CACHE_DIR/love-$LOVE_VERSION-win64.zip" "https://bitbucket.org/rude/love/downloads/love-$LOVE_VERSION-win64.zip"
        else
            curl -L -C - -o "$CACHE_DIR/love-$LOVE_VERSION-win-x64.zip" "https://bitbucket.org/rude/love/downloads/love-$LOVE_VERSION-win-x64.zip"
        fi
    fi

    unzip -qq "$CACHE_DIR/love-$LOVE_VERSION-win64.zip"

    if [[ -n $ICON ]]; then
        WINEPREFIX="$WINEPREFIX" wine "$RESHACKER" \
            -addoverwrite "love-$LOVE_VERSION-win64/love.exe,love-$LOVE_VERSION-win64/love.exe,$ICON,ICONGROUP,MAINICON,0" 2>&1 /dev/null
    fi

    cat love-$LOVE_VERSION-win64/love.exe "$LOVE_FILE" > "love-$LOVE_VERSION-win64/${TITLE}.exe"
    rm love-$LOVE_VERSION-win64/love.exe
    mv love-$LOVE_VERSION-win64 "$TITLE-win64"
    if [[ $INSTALLER == true ]]; then
        rm -rf "$IDENTITY-setup-win64.exe" 2> /dev/null
        create_installer "$TITLE-win64" "true"
        mv "$IDENTITY-setup.exe" "$IDENTITY-setup-win64.exe"
    else
        zip -FS -9 -qr "$TITLE-win64.zip" "$TITLE-win64"
    fi
    rm -rf "$TITLE-win64"
fi


exit_module
EndOfModule
))
default_module


elif [[ $INSTALLED == true ]]; then
    SCRIPTS_DIR="scripts"
    for file in "$SCRIPTS_DIR"/*.sh; do
        (source "$file")
        default_module
    done
fi


(
    init_module "LÖVE" "love" "L"
    create_love_file 9
    exit_module
)
if [[ $? -ne 0 && $DEFAULT_MODULE == true ]]; then
(
    init_module "LÖVE" "default"
    create_love_file 9
    exit_module
)
fi

exit 0

