#!/bin/bash
start=$(date)
SECONDS=0
echo "Script started at: $start"
export PYTHONUNBUFFERED="1"

# Run all steps if "all" is passed
if [ "$1" = "all" ]; then
    echo "Running full pipeline..."
    "$0" 1 1 99 && "$0" 2 1 99 && "$0" 3 1 99 && "$0" 4 1 99
    exit $?
fi

if [ "$#" -lt 2 ] || [ "$#" -gt 3 ]; then
    echo "Usage: $0 all                              # Run entire pipeline"
    echo "       $0 <folder_index> <script_start> [script_end]"
    exit 1
fi
folder_index="$1"
script_start="$2"
script_end="${3:-$script_start}"

# Main script execution loop
find src -type d -name "${folder_index}*" | while read -r folder; do
    find "$folder" -type f \( -name "*.R" -o -name "*.py" \) -not -path "*/archive/*" | sort -V | while read -r file; do
        filename=$(basename "$file")
        script_prefix=$(echo "$filename" | cut -d_ -f1)
        if ! [[ "$script_prefix" =~ ^[0-9]+$ ]]; then
            continue
        fi
        if (( script_prefix < script_start || script_prefix > script_end )); then
            continue
        fi
        echo -n "Running $file "
        # Get subfolder relative to src/
        src_subdir=$(echo "$folder" | sed 's|^src/||')
        log_directory="logs/$src_subdir"
        summary_directory="summary/$src_subdir"
        artifacts_directory="artifacts/$src_subdir"
        mkdir -p "$log_directory" "$summary_directory" "$artifacts_directory"
        log_file="$log_directory/${filename}.log"
        rm -f "$log_file"
        case "$file" in
            *.py)
                interpreter="python"
                ;;
            *.R)
                interpreter="Rscript"
                ;;
            *)
                echo "Unknown file type: $file"
                continue
                ;;
        esac

        # Run the interpreter in background and capture PID
        "$interpreter" "$file" >> "$log_file" 2>&1 &
        script_pid=$!
        echo "PID: $script_pid"

        # Wait for it to complete and check exit status
        wait $script_pid
        if [ $? -eq 0 ]; then
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