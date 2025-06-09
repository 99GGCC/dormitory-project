# Dormitory Management System  

## Overview  
This project is a **Dormitory Management System** built with **Spring Boot** (backend), **Vue.js** (frontend), and **SaToken** (authentication). It provides an efficient way to manage student dormitories, including room allocation, student check-ins/check-outs, maintenance requests, and administrative oversight.  

## Features  
- **User Authentication**: Secure login using **SaToken** for role-based access (admin, staff, students).  
- **Room Management**: Assign/unassign students to dorm rooms, track occupancy status.  
- **Maintenance Tracking**: Submit and manage repair requests.  
- **Dashboard & Reporting**: Visualize occupancy rates and generate reports.  
- **Responsive UI**: Vue.js frontend for seamless desktop/mobile use.  

## Tech Stack  
- **Backend**: Spring Boot (Java), MySQL, SaToken (JWT-based auth)  
- **Frontend**: Vue 3, Element Plus, Axios  
- **Tools**: Maven, Git, Swagger (API docs)  

## Setup  
1. **Backend**: Run `mvn spring-boot:run` (port `8080`).  
2. **Frontend**: Navigate to `/frontend`, run `npm install && npm run dev` (port `3000`).  

## License  
MIT. Contributions welcome!  

---
**Note**: Replace placeholders (e.g., database config) in `application.properties`. For details, see the [Wiki](https://github.com/99GGCC/dormitory-project/wiki).
