FROM node:10.16.3-alpine as build

WORKDIR /tmp/building
COPY package.json package-lock.json ./
RUN npm install
COPY . .
RUN npm run build

FROM node:10.16.3-alpine

WORKDIR /app
RUN npm install -g serve
COPY --from=build /tmp/building/build ./build
CMD ["serve", "-s", "build", "-l", "80"]