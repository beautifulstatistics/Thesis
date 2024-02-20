#!/bin/bash

start=$(date)
SECONDS=0

echo "Script started at: $start"

if [ -z "$1" ]
then
    echo "Please specify which files to run!"
    exit 1
fi

export PYTHONUNBUFFERED="1"

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

ls | grep -P "^$1" | while read -r line ;
do
    echo -n $line" "

    rm "./logs/"$line".log" 2> /dev/null

    case $line in
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

    "$interpreter" "$line" >> "./logs/$line.log" 2>&1 &
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

echo "Killing temp monitor."
kill $sensors_pid

echo "Killing mem monitor."
kill $mem_pid

end=$(date)
duration=$SECONDS

echo "Script ended at: $end"
echo "Total duration: $duration seconds."
