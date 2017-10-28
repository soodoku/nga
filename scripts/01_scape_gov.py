import urllib2
import csv
import re
from urlparse import urlparse
from BeautifulSoup import BeautifulSoup, SoupStrainer

# Initialize three tables
governor = csv.writer(open('governor.csv', 'wb'))
governor.writerow(('name', 'state', 'time in office', 'startoffice', 'endoffice', 'succeeded', 'resigned', 'diedinoffice', 'ngaChair', 'address', 'phone', 'fax', 'born', 'died', 'birth state', 'party', 'spouse', 'family', 'school', 'military service', 'bio', 'url', 'spouseurl', 'spousebio'))

mainsoup = BeautifulSoup(urllib2.urlopen('http://www.nga.org/cms/FormerGovBios?begincac77e09-db17-41cb-9de0-687b843338d0=0&higherOfficesServed=&lastName=&sex=Any&honors=&submit=Search&state=Any&college=&party=&inOffice=Any&biography=&race=Any&militaryService=&religion=&birthState=Any&firstName=&nbrterms=Any&warsServed=&&pagesizecac77e09-db17-41cb-9de0-687b843338d0=2378').read())

for new_host in mainsoup.findAll('a', href=re.compile('/cms/home/governors/')):
        
        # Vars. from the main page
        name = new_host.next
        state = new_host.findNext('td').string
        a = new_host.findNext('td').findNext('td').contents
        b = "".join(str(item) for item in a)
        timeInOffice = b.replace('<br />', '').replace(' ', '').strip()
        
        # Get Gov. webpage
        o = urlparse(new_host['href'])
        new_host = o.geturl()
        new_host = 'http://www.nga.org' + new_host
        print(new_host)
        soup = BeautifulSoup(urllib2.urlopen(new_host).read())
        url = new_host
        
        # Initialize vars
        succeeded = resigned = diedinoffice = ngaChair = startoffice = endoffice = ''
        phone = fax = address = born = died = birthstate = party = family = school = militaryservice = bio = spouseurl = spouse = spousebio = ''

        # Vars. from the gov. file
        succeeded = soup.find('li', {'id' : 'succeeded'})
        if succeeded.next.next.string != '': 
            succeeded = succeeded.next.next.string
            
        resigned = soup.find('li', {'id' : 'resigned'})
        if resigned.next.next.string != '': 
            resigned = resigned.next.next.string
        
        diedinoffice = soup.find('li', {'id' : 'diedInOffice'})
        if diedinoffice.next.next.string != '': 
            diedinoffice = diedinoffice.next.next.string
        
        ngaChair = soup.find('li', {'id' : 'ngaChair'})
        if ngaChair.next.next.string != '': 
            ngaChair = ngaChair.next.next.string
        
        startoffice = soup.find('span', {'id' : 'startOffice1'})
        if startoffice != None:
            startoffice = startoffice.string
        
        endoffice = soup.find('span', {'id' : 'endOffice1'})
        if endoffice != None:
            endoffice = endoffice.string
        
        phone = soup.find(text='Phone:')
        if phone != None:
            phone = phone.next.strip()
        
        fax = soup.find(text='Fax:')
        if fax != None:
            fax = fax.next.strip()
        
        address = soup.find(text='Address: ')
        if address != None:
            address = address.next.strip()
            
        born = soup.find(text='Born:').next.replace('&nbsp;', '')
        birthstate = soup.find(text='Birth State:').next.replace('&nbsp;', '').strip()
        
        party = soup.find(text='Party:')
        if party != None:
            party = party.next.replace('&nbsp;', '').strip()
        
        spouset = soup.find(text='Spouse:')
        if spouset != None:
            if spouset.next.next != None:
                if hasattr(spouset.next.next, 'name'):
                    spouseurl = 'http://www.nga.org' + spouset.next.next['href']
                    spsoup = BeautifulSoup(urllib2.urlopen(spouseurl).read())
                    tempbio = spsoup.find('div', {'class': 'article-body'})
                    a = list(tempbio('p'))
                    spousebio = re.sub(r'\[|\]|\p*<[^>]*>\p*', '', str(a)).replace('&nbsp;', '').strip()
                spouse = spouset.next.next.string
                
        family = soup.find(text='Family:')
        if family != None:
            family = family.next.replace('&nbsp;', '').strip()
        
        school = soup.find(text='School(s):')
        if school != None:
            school = school.next.replace('&nbsp;', '').strip()
            
        militaryservice = soup.find(text='Military Service:')
        if militaryservice != None:
            militaryservice = militaryservice.next.replace('&nbsp;', '').strip()
            
        tempbio = soup.find('div', {'class': 'article-body'})
        a = list(tempbio('p'))
        bio = re.sub(r'\[|\]|\p*<[^>]*>\p*', '', str(a)).replace('&nbsp;', '').strip()
        
        died = soup.find(text='Passed:')
        if died != None:
            died = died.next.replace('&nbsp;', '').strip()
        
        # Write row
        governor.writerow((name, state, timeInOffice, startoffice, endoffice, succeeded, resigned, diedinoffice, ngaChair, address, phone, fax, born, died, birthstate, party, spouse, family, school, militaryservice, bio, url, spouseurl, spousebio))         
