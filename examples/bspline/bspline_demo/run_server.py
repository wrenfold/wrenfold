from flask import Flask, redirect

app = Flask(__name__)


@app.route('/')
def hello():
    return redirect("/static/index.html", code=302)
