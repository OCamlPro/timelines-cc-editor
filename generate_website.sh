for file in static/pages/*.html; do
    page=${file:13:(-5)}
    echo "Creating $page"
    mkdir -p www/$page
    echo '<!DOCTYPE HTML> <html>' $(cat static/header.html) '<body> <div id="page_content-loading"></div><div id="page_content">' $(cat $file) '</div>' $(cat static/footer.html)  '</body> </html>' > www/$page/index.html
done

cp www/home/index.html www/index.html
