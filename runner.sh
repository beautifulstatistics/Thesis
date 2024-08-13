#!/bin/bash

start=$(date)
SECONDS=0

echo "Script started at: $start"

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <folder_prefix> <script_prefix>"
    exit 1
fi

folder_prefix="$1"
script_prefix="$2"

export PYTHONUNBUFFERED="1"

# Create logs directory if it doesn't exist
mkdir -p logs

(while true;
    do sensors |
       grep -oP '(?<=Core [0-9]:        \+)[0-9]{2}' |
       awk -v date="$(date)" '$1>82{c++}; END{if (c+0 > 1) print "Cores Over Temp: " c+0};';
       sleep 900;
    done) &

sensors_pid=$!
echo "Temp monitor PID: "$sensors_pid

(while true; 
    do 
       mem=$(free | grep 'Mem:');
       total_mem=$(echo $mem | awk '{print $2}');
       used_mem=$(echo $mem | awk '{print $3}');
       
       awk -v total=$total_mem -v used=$used_mem 'BEGIN{percent = (used/total)*100; if (percent > 90) print "RAM Over 90%: ", percent"%"}';
       
       sleep 900;
    done) &

mem_pid=$!
echo "Mem monitor PID: "$mem_pid

find src -type d -name "${folder_prefix}*" | while read -r folder; do
    find "$folder" -type f \( -name "${script_prefix}*.R" -o -name "${script_prefix}*.py" \) | sort | while read -r file; do
        echo -n "Running $file "

        directory=$(dirname "$file")
        filename=$(basename "$file")
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

        (cd "$directory" && "$interpreter" "$filename" > "../../$log_file" 2>&1) &
        interpreter_pid=$!
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

echo "Killing temp monitor."
kill $sensors_pid

echo "Killing mem monitor."
kill $mem_pid

end=$(date)
duration=$SECONDS

echo "Script ended at: $end"
echo "Total duration: $duration seconds."