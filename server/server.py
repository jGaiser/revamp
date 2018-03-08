import random
import rpy2
import rpy2.robjects as robjects
from flask import Flask, render_template

app = Flask(__name__, static_folder='../static/dist', template_folder='../static')

@app.route('/')
def index():
    return render_template('index.html', bromoTemplateID=BROMO_DOMAIN_TEMPLATE_ID)

#-----------------------STARTUP-----------------------#
# The following code is run upon starting the flask app

R_SCRIPT = "cy.R"
r = robjects.r
# Executes the provided R script
r.source(R_SCRIPT)
BROMO_DOMAIN_TEMPLATE_ID = r.buildBromoNetwork()[0]

if __name__ == '__main__':
    app.run(port=5001)
