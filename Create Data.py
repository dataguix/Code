import pandas as pd

import sys
import os

#Convert pdf to text
import textract
import PyPDF2
from PIL import Image
from pypdfocr.pypdfocr_gs import PyGs
import pytesseract
pytesseract.pytesseract.tesseract_cmd = 'Lib/site-packages/pytesseract/tesseract/tesseract'

import langdetect
import csv

import warnings
warnings.filterwarnings("ignore")


def RetrieveInfosCompany(path):
    fileName = path[path.find('/')+1:-4]
    words = fileName.split(' ')
    language = words[-1]
    year = words[-2]
    country = words[0]
    companyName = ''
    for i in range(len(words)-3):
        i += 1
        companyName += words[i] + ' '
    companyName = companyName[:-1]
    
    return country, companyName, year, language


def CountPagesNumber(path):
    pdfReader = PyPDF2.PdfFileReader(path)
    return pdfReader.numPages


def ConvertPdftoText(path):
    try:
        text = unicode(textract.process(path), 'utf-8')
        return text
    except ValueError:
        text = ""
        pagesNumber = CountPagesNumber(path)
        pdf = PyPDF2.PdfFileReader(path)
        if pdf.isEncrypted:
            pdf.decrypt('')
        for ith in range(pagesNumber):
            page = pdf.getPage(ith)
            text += page.extractText()
        text = unicode(text.encode('utf-8'), 'utf-8')
        return text
    
    
def ConvertScanToText(path, language):
    text = ''
    pagesNumber = CountPagesNumber(path)

    PyGs({}).make_img_from_pdf(path)

    for ith in range(pagesNumber):
        imagePath = path[:-4] + '_' + str(ith + 1) + '.JPG'
        image = Image.open(imagePath, mode='r')
        text += pytesseract.image_to_string(image, lang = language)
        os.remove(imagePath)

    return unicode(text.encode('utf-8'), 'utf-8')


def ConvertFileToText(path, language):
    text = ConvertPdftoText(path)
    scannedFile = 0
    pagesNumber = CountPagesNumber(path)
    
    if text in ['\x0c' * pagesNumber, '']:
        scannedFile = 1
        text = ConvertScanToText(path, language)
    
    languageEstimated = str(langdetect.detect_langs(text))
    
    return text, scannedFile, languageEstimated


def CreateData(directoryPath):
    sys.stdout.write("[" + len(os.listdir(directoryPath)) * " " + "] 0%")
    dataDic = []
    ithFile = 0
    for file in os.listdir(directoryPath):
        ithFile += 1
        path = directoryPath + '/' + file
        pagesNumber = CountPagesNumber(path)
        infosFile = RetrieveInfosCompany(path)
        infosText = ConvertFileToText(path, infosFile[3])
        
        dataDic.append({'Country': infosFile[0], 'Company': infosFile[1], 'Year': infosFile[2], 
                        'Text': infosText[0].encode('utf-8'), 
                        'Scan': infosText[1],'Pages Number': pagesNumber, 'Language Expected': infosFile[3], 
                        'Language Estimated': infosText[2]})
        
        percentage = int(ithFile * 100 / len(os.listdir(directoryPath)))
        spacesNumber = len(os.listdir(directoryPath)) - ithFile
        sys.stdout.write("\r")
        sys.stdout.write("[" + ithFile * "#" + spacesNumber * " " + "] " + str(percentage) + "%")
        
    df = pd.DataFrame(dataDic)
    
    df.to_csv('data.csv')
    print('\n CSV created')
    
    
CreateData('Annual reports')