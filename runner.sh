# !/bin/bash


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
    do swap=$(free | grep 'Swap:');
       swap_value=$(echo $swap | awk '{print $2}');
       awk -v date="$(date)" -v swap=$swap_value 'BEGIN{if (swap < 5*1024*1024) print "Swap Over Limit: " swap*1024*1024 " Gi"}';
       sleep 900; 
    done) &

swap_pid=$!
echo "Mem monitor PID: "$swap_pid

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

    if time "$interpreter" "$line" >> "./logs/$line.log"; then
        echo "Finished"
    else
        echo "Error. Aborting."
        exit 1
    fi

done

echo "Killing temp monitor."
kill $sensors_pid

echo "Killing mem monitor."
kill $swap_pid
