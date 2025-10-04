package pack.certs;

import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

@Component
public class CertificateInit implements CommandLineRunner {

  private final CertificateRepository repo;

  public CertificateInit(CertificateRepository repo) {
    this.repo = repo;
  }

  @Override
  public void run(String... args) {
    // 스모크 데이터 1건 삽입
    repo.save(new Certificate("정보처리기사", "IT"));
    System.out.println("Certificate count = " + repo.count());
  }
}
