import argparse
from selenium import webdriver
from selenium.webdriver.common.by import By
import pandas
from tqdm import tqdm

XPATH = "/html/body/div/main/div[2]/section[2]/div[3]/div/div[2]/div[1]/div[2]"
class Leaderboard:
    def __init__(self, path: str):
        self.path = path
        self.data = pandas.read_csv(path)
        self._init_driver()

    def _init_driver(self, headless: bool = True):
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
            params = self.driver.find_element(By.XPATH, XPATH)
            return params.text.split(" ")[0]
        except Exception as e:
            print(e)
            return "error"

    def add_nparams(self):
        self.data['nparams'] = [pandas.NA for _ in range(len(self.data))]
        for i, row in tqdm(self.data.iterrows(), total=len(self.data)):
            link = str(row['link'])
            nparams = self._get_nparams(link)
            self.data.at[i, 'nparams'] = nparams

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--path', type=str, default='/Users/alex/metabench/scraping/open-llm-leaderboard.csv')
    args = parser.parse_args()
    leaderboard = Leaderboard(args.path)
    leaderboard.add_nparams()
    leaderboard.data.to_csv(args.path, index=False)
    print(leaderboard.data['nparams'][:5])
    leaderboard.driver.quit()

if __name__ == '__main__':
    main()
