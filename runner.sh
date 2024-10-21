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

# Start temperature monitor in the background
(
    while true; do
        if ! command -v sensors >/dev/null 2>&1; then
            echo "sensors command not found. Please install lm-sensors."
            exit 1
        fi

        over_temp_cores=0
        while read -r line; do
            if [[ $line =~ ^Core\ [0-9]+: ]]; then
                temp=$(echo "$line" | awk '{print $3}' | tr -d '+°C')
                temp=${temp%.*}
                if (( temp > 82 )); then
                    ((over_temp_cores++))
                fi
            fi
        done < <(sensors)

        if (( over_temp_cores > 1 )); then
            echo "$(date '+%Y-%m-%d %H:%M:%S') - Cores Over Temp: $over_temp_cores"
        fi

        sleep 900
    done
) 2>&1 &
sensors_pid=$!
echo "Temp monitor PID: $sensors_pid"

# Start memory monitor in the background
(
    while true; do
        if ! command -v free >/dev/null 2>&1; then
            echo "free command not found."
            exit 1
        fi

        mem_info=$(free -m | awk '/^Mem:/ {print $2, $3}')
        total_mem=$(echo $mem_info | awk '{print $1}')
        used_mem=$(echo $mem_info | awk '{print $2}')

        if [ -z "$total_mem" ] || [ -z "$used_mem" ]; then
            echo "Could not retrieve memory information."
            sleep 900
            continue
        fi

        percent=$(awk -v total="$total_mem" -v used="$used_mem" 'BEGIN{printf "%.2f", (used/total)*100}')
        if (( $(echo "$percent > 90" | bc -l) )); then
            echo "$(date '+%Y-%m-%d %H:%M:%S') - RAM Over 90%: $percent%"
        fi

        sleep 900
    done
) 2>&1 &
mem_pid=$!
echo "Mem monitor PID: $mem_pid"

find src -type d -name "${folder_prefix}*" | while read -r folder; do
    find "$folder" -type f \( -name "${script_prefix}*.R" -o -name "${script_prefix}*.py" \) -not -path "*/archive/*" | sort | while read -r file; do
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

        # Run the interpreter directly and capture its PID
        cd "$directory"
        "$interpreter" "$filename" > "../../$log_file" 2>&1 &
        interpreter_pid=$!
        cd - >/dev/null

        echo "PID: $interpreter_pid"

        # Wait for the interpreter process to finish
        wait $interpreter_pid
        wait_exit_status=$?

        if [ $wait_exit_status -eq 0 ]; then
            echo "Finished"
        else
            echo "Error. Aborting."

            # Kill the monitors before exiting
            if kill -0 $sensors_pid 2>/dev/null; then
                echo "Killing temp monitor."
                kill $sensors_pid
            else
                echo "Temp monitor not running."
            fi

            if kill -0 $mem_pid 2>/dev/null; then
                echo "Killing mem monitor."
                kill $mem_pid
            else
                echo "Mem monitor not running."
            fi

            exit 1
        fi

    done
done

# Kill the monitors at the end
if kill -0 $sensors_pid 2>/dev/null; then
    echo "Killing temp monitor."
    kill $sensors_pid
else
    echo "Temp monitor not running."
fi

if kill -0 $mem_pid 2>/dev/null; then
    echo "Killing mem monitor."
    kill $mem_pid
else
    echo "Mem monitor not running."
fi

end=$(date)
duration=$SECONDS

echo "Script ended at: $end"
echo "Total duration: $duration seconds."
