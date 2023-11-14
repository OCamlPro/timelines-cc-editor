for file in static/pages/*.html; do
    page=$(echo $file | cut -d'.' -f 1 | cut -d'/' -f 3)
    echo "Creating $page"
    mkdir -p www/$page
    echo $(cat static/header.html) $(cat $file) $(cat static/footer.html) > www/$page/index.html
done

cp www/home/index.html www/index.html
