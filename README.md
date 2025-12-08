Chat Application

Delphi (Object Pascal) 기반의 실시간 채팅 애플리케이션

### 프로젝트 소개
Delphi의 Socket 통신을 활용한 실시간 채팅 프로그램입니다. 회원가입, 로그인, 친구 관리, 채팅방 생성 및 실시간 메시지 송수신 기능을 제공합니다.

### 개발 목적

Socket 프로그래밍을 통한 실시간 통신 구현
FireDAC을 이용한 데이터베이스 연동
SMTP를 활용한 이메일 인증 시스템 구현
보안 강화를 위한 비밀번호 암호화(SHA + Salt)

### 기술 스택
Frontend

Delphi VCL - GUI 프레임워크

Backend

ServerSocket/ClientSocket - TCP/IP 소켓 통신
FireDAC - 데이터베이스 액세스
Indy SMTP - 이메일 인증 발송

Database

MySQL - 사용자, 채팅방, 메시지 데이터 관리

Security

SHA Hash - 비밀번호 암호화
Salt - 레인보우 테이블 공격 방지

### 프로젝트 구조
```
chat/
├── Unit1.pas          # 채팅창
├── Unit2.pas          # 로그인
├── Unit3.pas          # 회원가입
├── Unit4.pas          # 이메일 인증
├── Unit5.pas          # 인증 번호 입력
├── Unit6.pas          # 비밀번호 재설정
├── Unit7.pas          # 메인 화면
├── Unit8.pas          # 채팅방 생성
├── Unit9.pas          # 채팅방 정보
├── Unit10.pas         # 마이페이지
├── Unit11.pas         # 본인 확인
├── Unit12.pas         # 서버
├── Unit13.pas         # 방 인원 선택 (친구 선택)
├── Unit14.pas         # 친구 목록
├── Unit16.pas         # 친구 요청
└── Unit17.pas         # 친구 요청 확인/거절
```
### 데이터베이스 스키마
```
Users 테이블
sqlCREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    password VARCHAR(255) NOT NULL,  -- SHA + Salt 해시
    salt VARCHAR(100) NOT NULL,
    email VARCHAR(100) NOT NULL,
    is_logged_in TINYINT(1) DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
ChatRooms 테이블
sqlCREATE TABLE chat_rooms (
    id INT AUTO_INCREMENT PRIMARY KEY,
    room_name VARCHAR(100) NOT NULL,
    is_public TINYINT(1) DEFAULT 0,
    created_by INT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (created_by) REFERENCES users(id)
);
Messages 테이블
sqlCREATE TABLE messages (
    id INT AUTO_INCREMENT PRIMARY KEY,
    room_id INT NOT NULL,
    user_id INT NOT NULL,
    message TEXT NOT NULL,
    sent_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (room_id) REFERENCES chat_rooms(id),
    FOREIGN KEY (user_id) REFERENCES users(id)
);
Friends 테이블
sqlCREATE TABLE friends (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    friend_id INT NOT NULL,
    status ENUM('pending', 'accepted', 'rejected') DEFAULT 'pending',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (friend_id) REFERENCES users(id)
);
```
### 사전 요구사항

Delphi IDE (10.2 Tokyo 이상)
MySQL Server 8.0 이상
SMTP 서버 계정 (Gmail, Naver 등)

설치 및 실행
1. 데이터베이스 설정
```
sqlCREATE DATABASE chat_db CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE chat_db;
```
2. SMTP 설정
Unit4.pas에서 SMTP 설정 수정:
```
pascalIdSMTP1.Host := 'smtp.gmail.com';
IdSMTP1.Port := 587;
IdSMTP1.Username := 'your-email@gmail.com';
IdSMTP1.Password := 'your-app-password';
```
3. 서버 실행

Unit12.pas (서버) 프로젝트 실행
포트 설정 및 서버 시작

4. 클라이언트 실행

채팅 클라이언트 실행
서버 주소 입력 (localhost 또는 서버 IP)
회원가입 또는 로그인

사용자 경험

직관적인 UI (본인/상대방 메시지 구분)
친구 검색 기능으로 빠른 접근
입장/퇴장 알림으로 실시간 상태 파악
메인 화면에서 채팅방 바로 입장

실시간 통신

Socket 기반 즉각적인 메시지 전송
다중 클라이언트 동시 접속 지원
채팅방별 메시지 브로드캐스트

### 트러블슈팅
문제 1: SMTP 연결 오류
증상: 이메일 인증 발송 실패
해결책:

Gmail: 2단계 인증 후 앱 비밀번호 생성
Naver: SMTP 설정에서 POP3/IMAP 사용 허용
방화벽에서 SMTP 포트(587, 465) 허용 확인

문제 2: Socket 연결 실패
증상: 클라이언트가 서버에 연결되지 않음
해결책:

서버가 먼저 실행되었는지 확인
방화벽에서 해당 포트 허용
로컬 테스트: localhost 또는 127.0.0.1 사용

문제 3: 한글 깨짐
증상: 채팅 메시지의 한글이 깨짐
해결책:

Socket 전송 시 UTF-8 인코딩 사용
MySQL 테이블 charset을 utf8mb4로 설정
Delphi 프로젝트 인코딩 설정 확인

### 향후 개발 계획

 + 파일/이미지 전송 기능
 + 읽음 표시 (Read Receipt)
 + 메시지 검색 기능
 + 채팅방 초대 링크
 + 푸시 알림
 + 다크 모드
 + 이모티콘/스티커
 + 메시지 암호화 (E2E Encryption)

### 배운 점

+ Socket 프로그래밍을 통한 실시간 통신 구현
+ 보안을 고려한 사용자 인증 시스템 설계
+ SMTP를 활용한 이메일 인증 구현
+ 복잡한 GUI 애플리케이션 구조 설계

### 개선이 필요한 부분

+ 프로토콜 정의: 체계적인 메시지 프로토콜 설계 필요
+ 에러 처리: 네트워크 끊김 등 예외 상황 처리 강화
+ 성능 최적화: 대량 메시지 처리 시 성능 개선
+ 코드 구조: Unit 수가 많아 모듈화 필요
+ 테스트: 자동화된 테스트 코드 작성
