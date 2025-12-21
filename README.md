# Chat Application

Delphi (Object Pascal) 기반의 실시간 채팅 애플리케이션

### 프로젝트 소개
+ Delphi의 Socket 통신을 활용한 실시간 채팅 프로그램입니다. 회원가입, 로그인, 친구 관리, 채팅방 생성 및 실시간 메시지 송수신 기능을 제공합니다.

### 개발 목적

+ Socket 프로그래밍을 통한 실시간 통신 구현
+ FireDAC을 이용한 데이터베이스 연동
+ SMTP를 활용한 이메일 인증 시스템 구현
+ 보안 강화를 위한 비밀번호 암호화(SHA + Salt)

### 기술 스택
+ Frontend
    + Delphi VCL - GUI 프레임워크

+ Backend
    + ServerSocket/ClientSocket - TCP/IP 소켓 통신
    + FireDAC - 데이터베이스 액세스
    + Indy SMTP - 이메일 인증 발송

+ Database
    + MySQL - 사용자, 채팅방, 메시지 데이터 관리

+ Security
    + SHA Hash - 비밀번호 암호화
    + Salt - 레인보우 테이블 공격 방지

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
├── Unit16.pas         # 친구 요청/추천 친구
└── Unit17.pas         # 친구 요청 확인/거절, 보낸 친구 요청 확인/취소
```
### 데이터베이스 스키마
```
create table user(
userno int AUTO_INCREMENT primary key ,
id varchar(20) not null unique ,
pw varchar(255) not null,
salt varchar(255),
name varchar(100) not null,
email varchar(30) not null check(email like '%@%'),
role varchar(20) default 'user', /* 권한 user,admin */  
is_logged_in TINYINT(1) DEFAULT 0 COMMENT '로그인 상태',
session_id VARCHAR(100) NULL COMMENT '세션 ID',
login_time DATETIME NULL COMMENT '로그인 시간',
last_activity DATETIME NULL COMMENT '마지막 활동 시간',
created_at DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT '계정 생성일',
updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '수정일',
is_active TINYINT(1) DEFAULT 1 COMMENT '계정 활성화',
readmessage int(1) comment '메시지 읽음 처리'
);

CREATE TABLE friend(
  request_id INT AUTO_INCREMENT PRIMARY KEY,
  requester_id VARCHAR(20) COMMENT '친구 요청을 보낸 user id (A)',
  receiver_id VARCHAR(20) COMMENT '친구 요청을 받은 user id (B)',
  status INT DEFAULT 0 COMMENT '0: 대기, 1: 초대 보냄, 2: 초대 수락(친구 상태)',
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT '요청 시간',
  CHECK(status < 3 AND status >= 0),
  FOREIGN KEY (requester_id) REFERENCES user(id) ON DELETE CASCADE,
  FOREIGN KEY (receiver_id) REFERENCES user(id) ON DELETE CASCADE,
  -- 중복 친구 관계 방지
  UNIQUE KEY unique_friendship (requester_id, receiver_id)
);

create table chat(
ChatRoomId int AUTO_INCREMENT primary key, /* 방 번호 */
num int default 1/* 인원수 */,
ChatType int not null check (ChatType in(1, 2)) default 2, /* 1: 일반 채팅(비번 있음) , 2 : 공개 채팅 비번 x */
chatpw varchar(50) default '',
chatroomname varchar(50) not null
);

create table chating(
c_no int AUTO_INCREMENT primary key,
ChatRoomId int not null,
userno int,
contents varchar(100), /* 글 내용 */
nowtime datetime default current_timestamp /* 현재 시간 */,
readchat int comment '읽은 유저 수',
img varchar(255),	
file varchar(255),
day varchar(100)
,foreign key(ChatRoomId) references chat(ChatRoomId)
,foreign key(userno) references user(userno)
);

CREATE TABLE chat_user (
  ChatRoomId INT NOT NULL,
  UserNo INT NOT NULL,
  joined_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  pin int default 0 comment '0: 기본상태, 1: 상단 고정 상태',
  pin_updated DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'pin 변경 시간',
  PRIMARY KEY (ChatRoomId, UserNo),
  FOREIGN KEY (ChatRoomId) REFERENCES chat(ChatRoomId),
  FOREIGN KEY (UserNo) REFERENCES user(UserNo)
);

CREATE TABLE chat_user_read (
  ChatRoomId INT NOT NULL,
  UserNo INT NOT NULL,
  last_read_message_id INT DEFAULT 0 COMMENT '마지막으로 읽은 메시지 ID (-1: 모두 읽음, 0: 읽은 메시지 없음)',
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '업데이트 시간',
  PRIMARY KEY (ChatRoomId, UserNo),
  FOREIGN KEY (ChatRoomId) REFERENCES chat(ChatRoomId) ON DELETE CASCADE,
  FOREIGN KEY (UserNo) REFERENCES user(userno) ON DELETE CASCADE
);

```
### 사전 요구사항

+ Delphi IDE (10.2 Tokyo 이상)
+ MySQL Server 8.0 이상
+ SMTP 서버 계정 (Gmail, Naver 등)

설치 및 실행
1. 데이터베이스 설정
```
sqlCREATE DATABASE chat_db CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE chat_db;
```
2. SMTP 설정
+ Unit4.pas에서 SMTP 설정 수정:
```
pascalIdSMTP1.Host := 'smtp.gmail.com';
IdSMTP1.Port := 587;
IdSMTP1.Username := 'your-email@gmail.com';
IdSMTP1.Password := 'your-app-password';
```
3. 서버 실행

+ Unit12.pas (서버) 프로젝트 실행
+ 포트 설정 및 서버 시작

4. 클라이언트 실행

+ 채팅 클라이언트 실행
+ 서버 주소 입력 (localhost 또는 서버 IP)
+ 회원가입 또는 로그인

+ 사용자 경험

    + 직관적인 UI (본인/상대방 메시지 구분)
    + 친구 검색 기능으로 빠른 접근
    + 입장/퇴장 알림으로 실시간 상태 파악
    + 메인 화면에서 채팅방 바로 입장

+ 실시간 통신

    + Socket 기반 즉각적인 메시지 전송
    + 다중 클라이언트 동시 접속 지원
    + 채팅방별 메시지 브로드캐스트

### 트러블슈팅
문제 1: SMTP 연결 오류
증상: 이메일 인증 발송 실패
해결책:
+ Gmail: 2단계 인증 후 앱 비밀번호 생성
+ Naver: SMTP 설정에서 POP3/IMAP 사용 허용
+ 방화벽에서 SMTP 포트(587, 465) 허용 확인

문제 2: Socket 연결 실패
증상: 클라이언트가 서버에 연결되지 않음
해결책:
+ 서버가 먼저 실행되었는지 확인
+ 방화벽에서 해당 포트 허용
+ 로컬 테스트: localhost 또는 127.0.0.1 사용

문제 3: 한글 깨짐
증상: 채팅 메시지의 한글이 깨짐
해결책:
+ Socket 전송 시 UTF-8 인코딩 사용
+ MySQL 테이블 charset을 utf8mb4로 설정
+ Delphi 프로젝트 인코딩 설정 확인

문제 4: Abstract Error
증상 : 내가 보낸 친구 요청을 삭제할 때 오류 발생
해결책 :
+ 기존 코드
```
  if ScrollBox2.ComponentCount > 0 then
  begin
  ScrollBox2.DestroyComponents;
end;
```
+ 수정 코드
```
    try
      while ScrollBox2.ControlCount > 0 do
      begin
        if ScrollBox2.Controls[0] <> nil then
          ScrollBox2.Controls[0].Free;
      end;
    except
      
    end;
```
+ Try-Except로 안전하게 처리
+ if ScrollBox2.Controls[0] <> nil then 로 포인터 검증
  
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
