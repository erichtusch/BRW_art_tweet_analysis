import os
from googleapiclient.discovery import build
from httplib2 import Http
from oauth2client import file, client, tools

# Artist spreadsheet ID
sheet_id = '18QERNDzlOBsH162oeKn--JmwTSvbOuVV0sNjKm3xUo0'

# Log in to Google sheets
scopes = 'https://www.googleapis.com/auth/spreadsheets.readonly'
store = file.Storage('token.json')
creds = store.get()
if not creds or creds.invalid:
    flow = client.flow_from_clientsecrets('credentials.json', scopes)
    creds = tools.run_flow(flow, store)
service = build('sheets', 'v4', http=creds.authorize(Http()))
sheet = service.spreadsheets().values().get(spreadsheetId=sheet_id, range='A2:E').execute()
values = sheet.get('values', [])

if not values:
    print('Error loading spreadsheet.')
else:
    # Scrape all tweets for each artist
    for row in values:
        artist = '\"%s %s\"' % (row[1], row[0])
        file = '%s%s.json' % (row[1], row[0])
        if not os.path.isfile(file):
            command = 'twitterscraper %s -o %s' % (artist, file)
            os.system(command)
