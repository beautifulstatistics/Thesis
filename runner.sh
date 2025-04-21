#!/bin/bash

start=$(date)
SECONDS=0

echo "Script started at: $start"

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <folder_index> <script_start> <script_end>"
    exit 1
fi

folder_index="$1"
script_start="$2"
script_end="$3"

export PYTHONUNBUFFERED="1"
mkdir -p logs

find src -type d -name "${folder_index}*" | while read -r folder; do
    find "$folder" -type f \( -name "*.R" -o -name "*.py" \) -not -path "*/archive/*" | sort | while read -r file; do
        filename=$(basename "$file")
        script_prefix=$(echo "$filename" | cut -d_ -f1)

        if ! [[ "$script_prefix" =~ ^[0-9]+$ ]]; then
            continue
        fi

        if (( script_prefix < script_start || script_prefix > script_end )); then
            continue
        fi

        echo -n "Running $file "

        directory=$(dirname "$file")
        log_file="logs/${directory//\//_}_$filename.log"
        rm "$log_file" 2> /dev/null

        case $file in
            *.py)
                interpreter="python"
                ;;
            *.R)
                interpreter="Rscript"
                ;;
            *)
                continue
                ;;
        esac

        cd "$directory"
        "$interpreter" "$filename" > "../../$log_file" 2>&1 &
        interpreter_pid=$!
        cd - >/dev/null

        echo "PID: $interpreter_pid"

        wait $interpreter_pid
        wait_exit_status=$?

        if [ $wait_exit_status -eq 0 ]; then
            echo "Finished"
        else
            echo "Error. Aborting."
            exit 1
        fi
    done
done

end=$(date)
duration=$SECONDS

echo "Script ended at: $end"
echo "Total duration: $duration seconds."
