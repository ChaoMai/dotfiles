#!/usr/bin/env bash

function set_ts() {
    local f=$1
    dto=$($GNUBIN_COREUTILS/date -d "$dir_t" "+%Y:%m:%d 00:00:00")
    echo exiftool -datetimeoriginal="$dto" "$f"
    exiftool -datetimeoriginal="$dto" "$f"

    local dest_path=$dest_dir/$dir_d

    if [[ ! -d "$dest_path" ]]; then
        echo mkdir "$dest_path"
        mkdir "$dest_path"
    fi

    echo mv "$f" "$dest_path"
    mv "$f" "$dest_path"
}

function process() {
    local dest_dir="/Users/chaomai/Downloads/final_imgs"

    local f=$1
    echo $f

    local dir_name=$(dirname "$f")
    local dir_d=$(basename "$dir_name")
    local dir_t=$(echo "$dir_d" | egrep -o '[0-9]{4}-[0-9]{2}-[0-9]{2}')

    if [[ ! -z "$dir_t" ]]; then
        local d=$(exiftool -DateTimeOriginal "$f")
        if [[ -z "$d" ]]; then
            echo "no image time for $f"
            set_ts "$f" "$dir_t"
        else
            local dest_path=$dest_dir/$dir_d

            if [[ ! -d "$dest_path" ]]; then
                echo mkdir "$dest_path"
                mkdir "$dest_path"
            fi

            echo mv "$f" "$dest_path"
            mv "$f" "$dest_path"
        fi

        echo ""
    fi
}

find ./takeout_imgs -type f -name '*.json' -delete
find ./takeout_imgs -type f -name '*.html' -delete

export -f process
export -f set_ts

find ./takeout_imgs -type f -name '*.mp4' -print0 | xargs -0 -n 1 -P 1 -I {} bash -c 'process "$@"' _ {}
