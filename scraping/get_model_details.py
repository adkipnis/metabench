import argparse
import json
from selenium import webdriver
from selenium.webdriver.common.by import By
import pandas
from tqdm import tqdm
HEADLESS = True
XPATH_P = "/html/body/div/main/div[2]/section[2]/div[3]/div/div[2]/div[1]/div[2]"

class Leaderboard:
    def __init__(self, path: str):
        self.path = path
        self.logpath = path.replace(".csv", "_log.txt")
        self.data = pandas.read_csv(path)
        self._init_driver()

    def _init_driver(self, headless: bool = HEADLESS):
        options = webdriver.ChromeOptions()
        if headless:
            options.add_argument("--headless=new")
        self.driver = webdriver.Chrome(options=options)

    def _get_nparams(self, link: str) -> str:
        self.driver.get(link)
        # check if page exists
        if "404" in self.driver.title:
            return "offline"
        try:
            params = self.driver.find_element(By.XPATH, XPATH_P)
            return params.text.split(" ")[0]
        except Exception as e:
            with open(self.logpath, 'a') as f:
                f.write(f"Link: {link}\nError: {e}\n\n")
            return "error"

    def add_nparams(self):
        self.data['nparams'] = [pandas.NA for _ in range(len(self.data))]
        for i, row in tqdm(self.data.iterrows(), total=len(self.data)):
            link = str(row['link'])
            nparams = self._get_nparams(link)
            self.data.at[i, 'nparams'] = nparams

    def _get_arch(self, link: str) -> str:
        json_link = link + "/raw/main/config.json"
        self.driver.get(json_link)
        # check if page exists
        if "404" in self.driver.title:
            return "offline"
        try:
            # parse json to dict
            config = self.driver.find_element(By.TAG_NAME, 'pre').text
            config = json.loads(config)
            arch = config['architectures']
            if len(arch) > 1:
                print(f"found {len(arch)} architectures, concatenating...")
            return ' '.join(arch)

        except Exception as e:
            with open(self.logpath, 'a') as f:
                f.write(f"Link: {link}\nError: {e}\n\n")
            return "error"

    def add_arch(self):
        self.data['arch'] = [pandas.NA for _ in range(len(self.data))]
        for i, row in tqdm(self.data.iterrows(), total=len(self.data)):
            link = str(row['link'])
            arch = self._get_arch(link)
            self.data.at[i, 'arch'] = arch

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--path', type=str, default='/Users/alex/metabench/scraping/open-llm-leaderboard.csv')
    args = parser.parse_args()
    leaderboard = Leaderboard(args.path)
    print(f"Loaded leaderboard with {len(leaderboard.data)} models")

    # print("Scraping number of params...")
    # leaderboard.add_nparams()
    
    print("Scraping architecture...")
    leaderboard.add_arch()

    print(f"Saving to {args.path}")
    leaderboard.data.to_csv(args.path, index=False)
    leaderboard.driver.quit()

if __name__ == '__main__':
    main()
