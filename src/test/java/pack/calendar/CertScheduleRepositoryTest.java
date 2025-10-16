package pack.calendar;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import java.util.List;
import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest
class CertScheduleRepositoryTest {

  @Autowired CertScheduleRepository repo;

  @Test
  void findByCertIdOrderByYearAscTermAsc() {
    //List<CertSchedule> list = repo.findByCertIdOrderByYearAscTermAsc(1L);
    //assertThat(list).isNotNull();
    // 시드가 있다면 정렬/사이즈 등 검증
    // assertThat(list).isNotEmpty();
    // assertThat(list).isSortedAccordingTo(Comparator.comparing(CertSchedule::getYear).thenComparing(CertSchedule::getTerm));
  }
}
